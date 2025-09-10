-module(erlmur_SUITE).

-include_lib("common_test/include/ct.hrl").

-include("Mumble_gpb.hrl").
-include("erlmur.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
    logger:set_primary_config(level, debug),
    [{timetrap, {seconds, 10}}].

init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    CertPem = filename:join([DataDir, "cert.pem"]),
    KeyPem = filename:join([DataDir, "key.pem"]),
    application:set_env(erlmur, listen_port, 0, [{persistent, true}]),
    application:set_env(erlmur, cert_pem, CertPem, [{persistent, true}]),
    application:set_env(erlmur, key_pem, KeyPem, [{persistent, true}]),
    {ok, _} = application:ensure_all_started(mnesia),
    [{cert_pem, CertPem}, {key_pem, KeyPem} | Config].

end_per_suite(_Config) ->
    application:stop(mnesia),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    {ok, _} = application:ensure_all_started(erlmur),
    % Retrieve the actual listening port

    % Assuming 'erlmur_listener' is the name
    Port = ranch:get_port(erlmur_ssl),
    [{listen_port, Port} | Config].

end_per_testcase(_TestCase, _Config) ->
    application:stop(erlmur),
    ok.

groups() ->
    [
        {all_tests, [], [
            version_msg_test_case,
            authenticate_msg_test_case,
            ping_msg_test_case,
            permissionquery_msg_test_case,
            %userstate_msg_test_case,
            %userstats_msg_test_case,
            %userremove_msg_test_case,
            channelstate_msg_test_case
        ]}
    ].

all() ->
    [{group, all_tests}].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

version_msg_test_case(Config) ->
    Client = start_erlmur_client(Config),
    send_version(Client).

authenticate_msg_test_case(Config) ->
    Client = start_erlmur_client(Config),
    send_version(Client),
    send_authenticate(Client).

ping_msg_test_case(Config) ->
    Client = start_erlmur_client(Config),
    send_version(Client),
    send_authenticate(Client),
    send_ping(Client).

permissionquery_msg_test_case(Config) ->
    Client = start_erlmur_client(Config),
    send_version(Client),
    send_authenticate(Client),
    send_permissionquery(Client).

userstate_msg_test_case(Config) ->
    Client = start_erlmur_client(Config),
    send_version(Client),
    send_authenticate(Client),
    send_userstate(Client).

userstats_msg_test_case(Config) ->
    Client = start_erlmur_client(Config),
    send_version(Client),
    send_authenticate(Client),
    send_userstats(Client).

userremove_msg_test_case(Config) ->
    Client1 = start_erlmur_client(Config),
    Client2 = start_erlmur_client(Config),
    send_version(Client1),
    send_version(Client2),
    send_authenticate(Client1),
    send_authenticate(Client2),
    send_userremove(Client2, Client1).

channelstate_msg_test_case(Config) ->
    Client = start_erlmur_client(Config),
    send_version(Client),
    send_authenticate(Client),
    send_new_channel(Client, 0, "Test"),
    Channels = erlmur_channel_store:find({name, "Test"}),
    lists:foreach(
        fun(C) -> send_remove_channel(Client, C#channel.id) end,
        Channels
    ).

%%--------------------------------------------------------------------
%% Help functions
%%--------------------------------------------------------------------

start_erlmur_client(Config) ->
    ct:log("Start client"),
    Port = ?config(listen_port, Config),
    {ok, Socket} = ssl:connect("localhost", Port, [
        {verify, verify_none},
        {active, false},
        binary,
        {certfile, proplists:get_value(cert_pem, Config)},
        {keyfile, proplists:get_value(key_pem, Config)}
    ]),
    {Socket}.

stop_erlmur_client(_) ->
    ct:log("Stop client").

send_version({Socket}) ->
    ct:log("Send version"),
    <<V1Version:32>> = <<1:16, 2:8, 4:8>>,
    VersionMsg = erlmur_tcp_message:pack(#'Version'{version_v1 = V1Version}),
    ssl:send(Socket, VersionMsg),
    get_replies(Socket, ['Version']).

send_authenticate({Socket}) ->
    ct:log("Send authentication"),
    AuthenticateMsg = erlmur_tcp_message:pack(#'Authenticate'{}),
    ssl:send(Socket, AuthenticateMsg),
    get_replies(
        Socket,
        [
            'ChannelState',
            'UserState',
            'CryptSetup',
            'CodecVersion',
            'ServerSync'
        ]
    ).

send_ping({Socket}) ->
    ct:log("Send ping"),
    PingMsg = erlmur_tcp_message:pack(#'Ping'{}),
    ssl:send(Socket, PingMsg),
    get_replies(Socket, ['Ping']).

send_permissionquery({Socket}) ->
    ct:log("Send permission query"),
    PermissionqueryMsg = erlmur_tcp_message:pack(#'PermissionQuery'{channel_id = 0}),
    ssl:send(Socket, PermissionqueryMsg),
    get_replies(Socket, ['PermissionQuery']).

send_userstate({Socket}) ->
    ct:log("Send user state"),
    UserStateMsg = erlmur_tcp_message:pack(#'UserState'{}),
    ssl:send(Socket, UserStateMsg),
    get_replies(Socket, ['UserState']).

send_userstats({Socket}) ->
    ct:log("Send user stats"),
    UserStatsMsg = erlmur_tcp_message:pack(#'UserStats'{}),
    ssl:send(Socket, UserStatsMsg),
    get_replies(Socket, ['UserStats']).

send_userremove({Socket}, {PidToRemove, _}) ->
    ct:log("Send user remove"),
    {ok, SessionRecord} = erlmur_session_registry:lookup({session_pid, PidToRemove}),
    UserRemoveMsg =
        erlmur_tcp_message:pack(#'UserRemove'{session = SessionRecord#session_record.session_id}),
    ssl:send(Socket, UserRemoveMsg),
    get_replies(Socket, ['UserRemove']).

send_new_channel({Socket}, Parent, Name) ->
    ct:log("Send new channel ~p parent ~p", [Name, Parent]),
    ChannelStateMsg = erlmur_tcp_message:pack(#'ChannelState'{parent = Parent, name = Name}),
    ssl:send(Socket, ChannelStateMsg),
    get_replies(Socket, ['ChannelState']).

send_remove_channel({Socket}, ChannelId) ->
    ct:log("Send remove channel ~p", [ChannelId]),
    ChannelRemoveMsg = erlmur_tcp_message:pack(#'ChannelRemove'{channel_id = ChannelId}),
    ssl:send(Socket, ChannelRemoveMsg),
    get_replies(Socket, ['ChannelRemove']).

get_replies(_Socket, []) ->
    ok;
get_replies(Socket, Expected) ->
    ct:log("Expecting: ~p", [Expected]),
    {ok, Msg} = ssl:recv(Socket, 0),
    ct:log("Recieved Msg Bin ~p", [Msg]),
    Messages = erlmur_tcp_message:decode(Msg),
    StillExpecting = remove_recived(Messages, Expected),
    ct:log("Removed ~p Expecting ~p", [Messages, StillExpecting]),
    get_replies(Socket, StillExpecting).

remove_recived([], Expected) ->
    Expected;
remove_recived([Message | Rest], Expected) ->
    ct:log("Unpacked Msg ~p", [Message]),
    remove_recived(Rest, lists:delete(element(1, Message), Expected)).
