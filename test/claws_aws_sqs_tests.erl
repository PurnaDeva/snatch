-module(claws_aws_sqs_tests).

-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("snatch.hrl").

-define(RECV_WAIT, 1000).

claws_aws_sqs_send_message_test_() ->
    {foreach,
     fun setup/0,
     fun stop/1,
     [
        fun test_process_message/0,
        fun test_static_send_receive/0,
        fun test_snatch/0
     ]
    }.

setup() ->
    ok = claws_aws_sqs_tests_mocks:init([]),
    {ok, Pid} = claws_aws_sqs:start_link({local, claws_aws_sqs}, #aws_config{}, 1, 21000, [], claws_aws_sqs_tests_mocks, 20),
    Pid.

stop(Pid) ->
    claws_aws_sqs_tests_mocks:stop(),
    %% start_link/7 links the claw to the test process, so the exit signal
    %% would otherwise kill the runner and cancel every later test.
    unlink(Pid),
    exit(Pid, shutdown),
    application:stop(snatch).

test_process_message() ->
    Contents = "<iq id=\"test-bot\" to=\"alice@localhost\" from=\"bob@localhost/pc\" type=\"get\"><query/></iq>",
    Results = claws_aws_sqs_consumer:process_messages(
        [{messages, [[{body, Contents}, {receipt_handle,"YzUyYzA0ZTctMzYwM"}]]}],
        claws_aws_sqs_tests_mocks,
        "Test",
        #aws_config{}
    ),
    Via = #via{claws = claws_aws_sqs},
    [
        ?_assertMatch([{ok, Contents, Via}], Results)
    ].

test_static_send_receive() ->
    QueueName = <<"test-queue">>,
    Message = <<"<test-message/>">>,
    claws_aws_sqs:send(Message, QueueName),
    Results = claws_aws_sqs_tests_mocks:receive_message(QueueName, {}),
    [
        ?_assert(claws_aws_sqs_tests_mocks:was_message_sent(QueueName, Message)),
        ?_assert(lists:member(Message, Results))
    ].

test_snatch() ->
    Contents = <<"<iq id=\"test-bot\" to=\"alice@localhost\" from=\"bob@localhost/pc\" type=\"get\"><query/></iq>">>,
    {ok, _} = snatch:start_link(claws_aws_sqs, self()),
    ok = snatch:send(Contents),
    ok = snatch:received(Contents),
    snatch:stop(),
    [
        ?_assertMatch([{received, Contents, #via{claws = claws_aws_sqs}}|_],
            recv_all([]))
    ].

%% Utils
recv_all(Data) ->
    receive
        D -> recv_all([D|Data])
    after
        ?RECV_WAIT -> Data
    end.

%% A claw may legitimately be started with no queue configured. init/1 must
%% survive that instead of crashing and taking down the caller's supervisor.
claws_aws_sqs_missing_queue_test_() ->
    [
        {"undefined queue, no explicit config",
         ?_assertMatch({ok, _}, claws_aws_sqs:init([undefined]))},
        {"undefined queue, explicit config",
         ?_assertMatch({ok, _}, claws_aws_sqs:init([#aws_config{}, undefined]))},
        {"empty string queue",
         ?_assertMatch({ok, _}, claws_aws_sqs:init([""]))},
        {"empty binary queue",
         ?_assertMatch({ok, _}, claws_aws_sqs:init([<<>>]))},
        {"undefined queue schedules no poll",
         ?_test(begin
            {ok, _} = claws_aws_sqs:init(
                [#aws_config{}, 1, 10, undefined, claws_aws_sqs_tests_mocks, 20]),
            ?assertEqual(timeout,
                receive poll_sqs -> got_poll after 200 -> timeout end)
         end)},
        {"configured queue still schedules a poll",
         ?_test(begin
            {ok, _} = claws_aws_sqs:init(
                [#aws_config{}, 1, 10, "a-queue", claws_aws_sqs_tests_mocks, 20]),
            ?assertEqual(got_poll,
                receive poll_sqs -> got_poll after 200 -> timeout end)
         end)}
    ].

%% The region must still be derived from a well-formed queue URL.
claws_aws_sqs_region_from_url_test_() ->
    Url = "https://sqs.us-west-2.amazonaws.com/123456789012/my-queue",
    {ok, State} = claws_aws_sqs:init([Url]),
    %% #state{} is private to the module; aws_config is its first field.
    %% The match guards against that field order silently changing.
    #aws_config{} = Config = element(2, State),
    [
        {"sqs host pinned to the queue's region",
         ?_assertEqual("sqs.us-west-2.amazonaws.com", Config#aws_config.sqs_host)}
    ].
