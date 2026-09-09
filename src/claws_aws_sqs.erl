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

-record(state, {
    aws_config = "" :: erlcloud_aws:aws_config(),
    max_number_of_messages = 1 :: integer(),
    poll_interval = 21000 :: integer(),
    queue :: queue_name(),
    sqs_module :: module(),
    wait_timeout_seconds = 20 :: integer()
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
    AwsConfig0 =
        try erlcloud_aws:auto_config() of
            {ok, Config} -> Config;
            _ -> erlcloud_aws:default_config()
        catch _:_ ->
            erlcloud_aws:default_config()
        end,
    AwsConfig = configure_region_from_url(AwsConfig0, QueueName),
    init([AwsConfig, QueueName]);

init([AwsConfig, QueueName]) ->
    init([AwsConfig, 1, 21000, QueueName, erlcloud_sqs, 20]);

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

handle_cast({send, QueueName, Message}, #state{aws_config = AwsConfig, sqs_module = SqsModule} = State) ->
    case SqsModule:send_message(QueueName, Message, AwsConfig) of
        [{message_id, _MessageId}, {md5_of_message_body, _Md5OfMessageBody}] ->
            {noreply, State};
        ErrorInfo ->
            lager:error("error in SQS send_message/3 => ~p", [ErrorInfo]),
            {stop, {sqs_send_failed, ErrorInfo}, State}
    end;

handle_cast({send, QueueName, Data, Attributes}, #state{aws_config = AwsConfig, sqs_module = SqsModule} = State) ->
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

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(poll_sqs, #state{aws_config = AwsConfig, max_number_of_messages = MaxNumberOfMessages, poll_interval = PollInterval, queue = Queue, sqs_module = SqsModule, wait_timeout_seconds = WaitTimeoutSeconds} = State) ->
    Messages = SqsModule:receive_message(Queue, all, MaxNumberOfMessages, none, WaitTimeoutSeconds, AwsConfig),
    process_messages(Messages, SqsModule, Queue, AwsConfig),
    erlang:send_after(PollInterval, self(), poll_sqs),
    {noreply, State};

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
