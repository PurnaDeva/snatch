-module(claws_aws_sqs).

-behaviour(gen_server).
-behaviour(claws).

-include("snatch.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-record(state, {
    aws_config = "" :: erlcloud_aws:aws_config(),
    sqs_module :: module()
}).

-define(SQS_MESSAGE_ID, <<"message_id">>).
-define(SQS_BODY, <<"message_body">>).

%% API
-export([start_link/0, start_link/1, start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% claws callbacks
-export([send/2,
         send/3]).

%% Util functions (also used in tests)
-export([process_message/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    AwsConfig =
        try
            {ok, Config} = erlcloud_aws:auto_config(),
            Config
        catch _ ->
            erlcloud_aws:default_config()
        end,
    gen_server:start_link({local, ?MODULE}, ?MODULE, {AwsConfig}, []).

-spec start_link(aws_config()) -> {ok, pid()}.
start_link(AwsConfig) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {AwsConfig}, []).

-spec start_link(aws_config(), module()) -> {ok, pid()}.
start_link(AwsConfig, SqsModule) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {AwsConfig, SqsModule}, []).

%% Callbacks
init({AwsConfig})  ->
    init({AwsConfig, erlcloud_sqs});

init({AwsConfig, SqsModule}) ->
    {ok, #state{aws_config = AwsConfig, sqs_module = SqsModule}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send, QueueName, Message}, #state{aws_config = AwsConfig, sqs_module = SqsModule} = State) ->
    case SqsModule:send_message(QueueName, Message, AwsConfig) of
        {ok, _Response} ->
            {noreply, State};
        {error, Reason} ->
            io:format("error => ~p~n", [Reason]),
            {noreply, State}
    end;

handle_cast({send, QueueName, Data, Attributes}, #state{aws_config = AwsConfig, sqs_module = SqsModule} = State) ->
    SQSAttributes = lists:map(fun({Key, {DataType, Value}}) ->
                                  {binary_to_list(Key), [{data_type, DataType}, {string_value, Value}]}
                              end, Attributes),
    case SqsModule:send_message(QueueName, Data, [{message_attributes, SQSAttributes}], AwsConfig) of
        {ok, _Response} ->
            {noreply, State};
        {error, Reason} ->
            io:format("error => ~p~n", [Reason]),
            {noreply, State}
    end;

handle_cast({received, QueueUrl}, #state{aws_config = AwsConfig, sqs_module = SqsModule} = State) ->
    {ok, Messages} = SqsModule:receive_message(QueueUrl, AwsConfig),
    [received_messages(Message) || Message <- Messages],
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send(Data, JID) ->
    gen_server:cast(?MODULE, {send, JID, Data}).

send(Data, JID, ID) -> %% TODO not sure what to do with the ID in this context
    MessageAttributes = [{<<"Id">>, {string, ID}}],
    gen_server:cast(?MODULE, {send, JID, Data, MessageAttributes}).

%% Util
process_message(Message) ->
    case jsone:decode(Message, [return_maps]) of
        {ok, Json} ->
            Body = maps:get(Json, ?SQS_BODY),
            case fxml_stream:parse_element(Body) of
                {ok, XmlElement} ->
                    Via = #via{claws = ?MODULE},
                    TrimmedPacket = snatch_xml:clean_spaces(XmlElement),
                    {ok, TrimmedPacket, Via};
                {error, _Reason} ->
                    {error, xml_parsing_failed}
            end;
        {error, _Reason} ->
            {error, json_parsing_failed}
        end.

received_messages(Message) ->
    case process_message(Message) of
        {error, Reason} -> {error, Reason};
        {ok, TrimmedPacket, Via} -> snatch:received(TrimmedPacket, Via)
    end.
