-module(claws_aws_sqs).

-behaviour(gen_server).
-behaviour(claws).

-include("snatch.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

%% API
-export([start_link/1, start_link/2, start_link/3, start_link/7]).

%% gen_server callbacks
-export([code_change/3,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

%% claws callbacks
-export([send/2, send/3]).

%% A claw drains its queue: a full batch per receive, a long poll that returns
%% as soon as a message arrives, and the next receive as soon as the last one
%% returns. A failed receive or delete is retried after a pause rather than crashing the
%% claw, so a lasting AWS fault cannot restart it fast enough to take its
%% supervisor down.
-define(DEFAULT_MAX_NUMBER_OF_MESSAGES, 10).
-define(DEFAULT_POLL_INTERVAL, 0).
-define(DEFAULT_WAIT_TIMEOUT_SECONDS, 20).
-define(ERROR_BACKOFF, 20000).

%% Credentials the claw fetched itself (an instance role, via auto_config/0)
%% are temporary. They are fetched again this many seconds before they expire,
%% the margin within which the instance metadata service has already rotated
%% them, and after any failed poll, so the claw never signs with an expired
%% token for longer than one backoff.
-define(CREDENTIALS_REFRESH_MARGIN, 300).

-record(state, {
    aws_config = "" :: erlcloud_aws:aws_config(),
    fetch_config :: fun(() -> aws_config()) | undefined,
    max_number_of_messages = ?DEFAULT_MAX_NUMBER_OF_MESSAGES :: integer(),
    poll_interval = ?DEFAULT_POLL_INTERVAL :: integer(),
    queue :: queue_name(),
    sqs_module :: module(),
    wait_timeout_seconds = ?DEFAULT_WAIT_TIMEOUT_SECONDS :: integer()
}).

-type server_name() :: {local, atom()} | {global, term()} | {via, module(), term()}.

%% A claw may be started with no queue configured for this environment, in
%% which case the queue name is `undefined' or empty rather than a URL.
-type queue_name() :: string() | binary() | undefined.

-spec start_link(queue_name()) -> {ok, pid()}.
start_link(QueueName) ->
    gen_server:start_link(?MODULE, [QueueName], []).

-spec start_link(server_name(), queue_name()) -> {ok, pid()}.
start_link(ServerName, QueueName) ->
    gen_server:start_link(ServerName, ?MODULE, [QueueName], []).

-spec start_link(server_name(), aws_config(), queue_name()) -> {ok, pid()}.
start_link(ServerName, AwsConfig, QueueName) ->
    gen_server:start_link(ServerName, ?MODULE, [AwsConfig, QueueName], []).

-spec start_link(server_name(), aws_config(), integer(), integer(), queue_name(), module(), integer()) -> {ok, pid()}.
start_link(ServerName, AwsConfig, MaxNumberOfMessages, PollInterval, QueueNames, SqsModule, WaitTimeoutSeconds) ->
    Args = [AwsConfig, MaxNumberOfMessages, PollInterval, QueueNames, SqsModule, WaitTimeoutSeconds],
    gen_server:start_link(ServerName, ?MODULE, Args, []).

%% Callbacks
init([QueueName]) ->
    init([fun() -> fetch_config(QueueName) end, QueueName]);

init([AwsConfig, QueueName]) ->
    init([AwsConfig, ?DEFAULT_MAX_NUMBER_OF_MESSAGES, ?DEFAULT_POLL_INTERVAL,
          QueueName, erlcloud_sqs, ?DEFAULT_WAIT_TIMEOUT_SECONDS]);

%% The config is either fixed, or a function the claw calls again whenever
%% the credentials it returned are about to expire.
init([FetchConfig, MaxNumberOfMessages, PollInterval, QueueName, SqsModule, WaitTimeoutSeconds])
  when is_function(FetchConfig, 0) ->
    {ok, State} = init([FetchConfig(), MaxNumberOfMessages, PollInterval, QueueName, SqsModule, WaitTimeoutSeconds]),
    {ok, State#state{fetch_config = FetchConfig}};

init([AwsConfig, MaxNumberOfMessages, PollInterval, QueueName, SqsModule, WaitTimeoutSeconds]) ->
    State = #state{
        aws_config = AwsConfig,
        max_number_of_messages = MaxNumberOfMessages,
        poll_interval = PollInterval,
        queue = QueueName,
        sqs_module = SqsModule,
        wait_timeout_seconds = WaitTimeoutSeconds
    },
    case queue_configured(QueueName) of
        true -> erlang:send_after(PollInterval, self(), poll_sqs);
        false -> ok
    end,
    {ok, State}.

%% gen_server
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(Msg, State) ->
    handle_send(Msg, refresh_config(State, false)).

handle_send({send, QueueName, Message}, #state{aws_config = AwsConfig, sqs_module = SqsModule} = State) ->
    case SqsModule:send_message(QueueName, Message, AwsConfig) of
        [{message_id, _MessageId}, {md5_of_message_body, _Md5OfMessageBody}] ->
            {noreply, State};
        ErrorInfo ->
            lager:error("error in SQS send_message/3 => ~p", [ErrorInfo]),
            {stop, {sqs_send_failed, ErrorInfo}, State}
    end;

handle_send({send, QueueName, Data, Attributes}, #state{aws_config = AwsConfig, sqs_module = SqsModule} = State) ->
    SQSAttributes = lists:map(fun({Key, {DataType, Value}}) ->
                                  {binary_to_list(Key), [{data_type, DataType}, {string_value, Value}]}
                              end, Attributes),
    case SqsModule:send_message(QueueName, Data, [{message_attributes, SQSAttributes}], AwsConfig) of
        [{message_id, _MessageId}, {md5_of_message_body, _Md5OfMessageBody}] ->
            {noreply, State};
        ErrorInfo ->
            lager:error("error in SQS send_message/3 => ~p", [ErrorInfo]),
            {stop, {sqs_send_failed, ErrorInfo}, State}
    end;

handle_send(_Msg, State) ->
    {noreply, State}.

handle_info(poll_sqs, State0) ->
    #state{aws_config = AwsConfig, max_number_of_messages = MaxNumberOfMessages, poll_interval = PollInterval, queue = Queue, sqs_module = SqsModule, wait_timeout_seconds = WaitTimeoutSeconds} = State = refresh_config(State0, false),
    {NextPoll, NextState} =
        try
            Messages = SqsModule:receive_message(Queue, all, MaxNumberOfMessages, none, WaitTimeoutSeconds, AwsConfig),
            process_messages(Messages, SqsModule, Queue, AwsConfig),
            {PollInterval, State}
        catch Class:Reason ->
            %% A message handed on but not deleted comes back after its
            %% visibility timeout: SQS is at-least-once either way.
            lager:error("SQS poll of ~p failed, retrying in ~p ms: ~p:~p",
                        [Queue, ?ERROR_BACKOFF, Class, Reason]),
            {?ERROR_BACKOFF, refresh_config(State, true)}
        end,
    erlang:send_after(NextPoll, self(), poll_sqs),
    {noreply, NextState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Claws callbacks
send(Data, JID) ->
    gen_server:cast(?MODULE, {send, Data, JID}).

send(Data, JID, ID) ->
    gen_server:cast(?MODULE, {send, Data, JID, ID}).

%% Utils
process_messages(MessageList, SqsModule, QueueName, AwsConfig) ->
    Messages = proplists:get_value(messages, MessageList, []),
    lists:foreach(fun(M) ->
        Body = list_to_binary(proplists:get_value(body, M, "")),
        lager:debug("SQS Received message Body: ~p from Queue: ~p", [Body, QueueName]),
        case process_body(Body) of
            {ok, Packet} ->
                Receipt = proplists:get_value(receipt_handle, M),
                MessageID = proplists:get_value(message_id, M),
                snatch:received(Packet, #via{claws = ?MODULE, exchange = SqsModule, jid = QueueName, id = MessageID}),
                SqsModule:delete_message(QueueName, Receipt, AwsConfig);
            {error, Reason} ->
                lager:error("Failed to process message: ~p ~p", [Body, Reason])
        end
    end, Messages).

process_body(Body) ->
    case fxml_stream:parse_element(Body) of
        {error, Reason} ->
            try_parse_json(Body, Reason);
        Packet ->
            {ok, Packet}
    end.

try_parse_json(Body, XMLParseError) ->
    case jsone:try_decode(Body, []) of
        {ok, Packet, _} ->
            {ok, Packet};
        {error, {Reason, Stacktrace}} ->
            {error, {parsing_failed, [{xml_error, XMLParseError}, {json_error, {Reason, Stacktrace}}]}}
    end.

%% The config a claw started with only a queue URL fetches for itself.
fetch_config(QueueName) ->
    AwsConfig =
        try erlcloud_aws:auto_config() of
            {ok, Config} -> Config;
            _ -> erlcloud_aws:default_config()
        catch _:_ ->
            erlcloud_aws:default_config()
        end,
    configure_region_from_url(AwsConfig, QueueName).

%% Fetch the credentials again when they are about to expire, or always when
%% forced. A fetch that yields no credentials keeps the ones the claw has: they
%% may still be good, and a config without a key would certainly not be.
refresh_config(#state{fetch_config = undefined} = State, _Force) ->
    State;
refresh_config(#state{fetch_config = FetchConfig, aws_config = AwsConfig} = State, Force) ->
    case Force orelse expiring(AwsConfig) of
        false ->
            State;
        true ->
            case FetchConfig() of
                #aws_config{access_key_id = KeyId} = Fresh when is_list(KeyId), KeyId =/= [] ->
                    State#state{aws_config = Fresh};
                _ ->
                    lager:error("Fetching AWS credentials for ~p failed, keeping the current ones",
                                [State#state.queue]),
                    State
            end
    end.

expiring(#aws_config{expiration = Expiration}) when is_integer(Expiration) ->
    Expiration - erlang:system_time(second) < ?CREDENTIALS_REFRESH_MARGIN;
expiring(_AwsConfig) ->
    false.

%% Pin the SQS endpoint host to the queue URL's region so erlcloud signs
%% the request for that region instead of falling back to its us-east-1
%% default when no AWS_REGION / aws_region env is configured.
configure_region_from_url(AwsConfig, QueueUrl) ->
    case queue_configured(QueueUrl) of
        false -> AwsConfig;
        true -> region_config_from_url(AwsConfig, QueueUrl)
    end.

%% Anything that is not a well-formed URL leaves the config untouched. A queue
%% name is not guaranteed to be a URL, and a bad one must not crash init/1.
region_config_from_url(AwsConfig, QueueUrl) ->
    try uri_string:parse(QueueUrl) of
        #{host := Host} when Host =/= "", Host =/= <<>> ->
            HostStr = case is_binary(Host) of
                          true  -> binary_to_list(Host);
                          false -> Host
                      end,
            Region = erlcloud_aws:aws_region_from_host(HostStr),
            erlcloud_aws:service_config(<<"sqs">>, Region, AwsConfig);
        _ ->
            AwsConfig
    catch _:_ ->
        AwsConfig
    end.

%% True when a queue name was actually configured for this claw. A list or a
%% binary is taken as a name; anything else -- notably `undefined', which is
%% what an unset environment variable yields -- means no queue, and the claw
%% then sits idle instead of polling one that does not exist.
queue_configured(QueueName) when is_list(QueueName); is_binary(QueueName) ->
    not string:is_empty(QueueName);
queue_configured(_QueueName) ->
    false.
