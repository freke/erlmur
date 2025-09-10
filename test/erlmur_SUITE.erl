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
    Config.

end_per_suite(_Config) ->
    application:stop(mnesia),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    meck:new(ssl),
    start_erlmur(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    stop_erlmur(),
    meck:unload(ssl),
    ok.

groups() ->
    [].

all() ->
    [
        version_msg_test_case,
        authenticate_msg_test_case,
        ping_msg_test_case,
        permissionquery_msg_test_case,
        userstate_msg_test_case,
        userstats_msg_test_case,
        userremove_msg_test_case,
        channelstate_msg_test_case
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

version_msg_test_case() ->
    [].

authenticate_msg_test_case() ->
    [].

ping_msg_test_case() ->
    [].

permissionquery_msg_test_case() ->
    [].

userstate_msg_test_case() ->
    [].

userstats_msg_test_case() ->
    [].

userremove_msg_test_case() ->
    [].

channelstate_msg_test_case() ->
    [].

version_msg_test_case(_Config) ->
    Client = start_erlmur_client(),
    send_version(Client).

authenticate_msg_test_case(_Config) ->
    Client = start_erlmur_client(),
    send_authenticate(Client).

ping_msg_test_case(_Config) ->
    Client = start_erlmur_client(),
    send_version(Client),
    send_ping(Client).

permissionquery_msg_test_case(_Config) ->
    Client = start_erlmur_client(),
    send_permissionquery(Client).

userstate_msg_test_case(_Config) ->
    Client = start_erlmur_client(),
    send_authenticate(Client),
    send_userstate(Client).

userstats_msg_test_case(_Config) ->
    Client = start_erlmur_client(),
    send_authenticate(Client),
    send_userstats(Client).

userremove_msg_test_case(_Config) ->
    Client1 = start_erlmur_client(),
    Client2 = start_erlmur_client(),
    send_authenticate(Client1),
    send_authenticate(Client2),
    send_userremove(Client2, Client1).

channelstate_msg_test_case(_Config) ->
    Client = start_erlmur_client(),
    send_authenticate(Client),
    send_new_channel(Client, 0, "Test"),
    Channels = erlmur_channels:find({name, "Test"}),
    lists:foreach(
        fun(C) -> send_remove_channel(Client, C#channel.id) end,
        Channels
    ).

%%--------------------------------------------------------------------
%% Help functions
%%--------------------------------------------------------------------
start_erlmur() ->
    %% Setup ssl
    ct:log("Mocking SSL"),
    meck:expect(ssl, listen, fun(Port, Options) -> meck:passthrough([Port, Options]) end),
    meck:expect(
        ssl,
        transport_accept,
        fun(ListenSocket) -> meck:passthrough([ListenSocket]) end
    ),
    meck:expect(ssl, close, fun(_Socket) -> ok end),
    meck:expect(ssl, controlling_process, fun(_Socket, _Pid) -> ok end),
    meck:expect(ssl, handshake, fun(Socket) -> {ok, Socket} end),
    meck:expect(ssl, setopts, fun(_Socket, _Options) -> ok end),
    meck:expect(ssl, peername, fun(Socket) -> {ok, {Socket, port}} end),

    Server = self(),

    meck:expect(ssl, send, fun(Socket, Msg) ->
        Server ! {Socket, Msg},
        ok
    end),

    ct:log("Start Erlmur"),
    Status = application:ensure_all_started(erlmur),
    ct:log("Erlmur stated ~p", [Status]).

stop_erlmur() ->
    ct:log("Stop Erlmur"),
    application:stop(erlmur).

start_erlmur_client() ->
    ct:log("Start client"),
    Socket = make_ref(),
    {ok, Pid} = supervisor:start_child(erlmur_session_sup, [Socket]),
    {Pid, Socket}.

stop_erlmur_client({Pid, _}) ->
    ct:log("Stop client"),
    supervisor:terminate_child(erlmur_session_sup, Pid).

send_version({Pid, Socket}) ->
    ct:log("Send version"),
    <<V1Version:32>> = <<1:16, 2:8, 4:8>>,
    VersionMsg = erlmur_message:pack(#'Version'{version_v1 = V1Version}),
    Pid ! {ssl, self(), VersionMsg},
    get_replies(Socket, ['Version']).

send_authenticate({Pid, Socket}) ->
    ct:log("Send authentication"),
    AuthenticateMsg = erlmur_message:pack(#'Authenticate'{}),
    Pid ! {ssl, self(), AuthenticateMsg},
    get_replies(
        Socket,
        [
            'ChannelState',
            'UserState',
            'CryptSetup',
            'CodecVersion',
            'ServerConfig',
            'ServerSync'
        ]
    ).

send_ping({Pid, Socket}) ->
    ct:log("Send ping"),
    PingMsg = erlmur_message:pack(#'Ping'{}),
    Pid ! {ssl, self(), PingMsg},
    get_replies(Socket, ['Ping']).

send_permissionquery({Pid, Socket}) ->
    ct:log("Send permission query"),
    AuthenticateMsg = erlmur_message:pack(#'Authenticate'{}),
    Pid ! {ssl, self(), AuthenticateMsg},
    PermissionqueryMsg = erlmur_message:pack(#'PermissionQuery'{channel_id = 0}),
    Pid ! {ssl, self(), PermissionqueryMsg},
    get_replies(Socket, ['PermissionQuery']).

send_userstate({Pid, Socket}) ->
    ct:log("Send user state"),
    UserStateMsg = erlmur_message:pack(#'UserState'{}),
    Pid ! {ssl, self(), UserStateMsg},
    get_replies(Socket, ['UserState']).

send_userstats({Pid, Socket}) ->
    ct:log("Send user stats"),
    {ok, SessionRecord} = erlmur_session_registry:lookup({session_pid, Pid}),
    UserStatsMsg = erlmur_message:pack(#'UserStats'{
        session = SessionRecord#session_record.session_id
    }),
    Pid ! {ssl, self(), UserStatsMsg},
    get_replies(Socket, ['UserStats']).

send_userremove({Pid, Socket}, {PidToRemove, _}) ->
    ct:log("Send user remove"),
    {ok, SessionRecord} = erlmur_session_registry:lookup({session_pid, PidToRemove}),
    UserRemoveMsg =
        erlmur_message:pack(#'UserRemove'{session = SessionRecord#session_record.session_id}),
    Pid ! {ssl, self(), UserRemoveMsg},
    get_replies(Socket, ['UserRemove']).

send_new_channel({Pid, Socket}, Parent, Name) ->
    ct:log("Send new channel"),
    ChannelStateMsg = erlmur_message:pack(#'ChannelState'{parent = Parent, name = Name}),
    Pid ! {ssl, self(), ChannelStateMsg},
    get_replies(Socket, ['ChannelState']).

send_remove_channel({Pid, Socket}, ChannelId) ->
    ct:log("Send remove channel"),
    ChannelRemoveMsg = erlmur_message:pack(#'ChannelRemove'{channel_id = ChannelId}),
    Pid ! {ssl, self(), ChannelRemoveMsg},
    get_replies(Socket, ['ChannelRemove']).

get_replies(_Socket, []) ->
    ok;
get_replies(Socket, Expected) ->
    logger:info("Expecting: ~p", [Expected]),
    receive
        {Socket, Msg} ->
            ct:log("Recieved Msg Bin ~p", [Msg]),
            [Message] = erlmur_message:unpack(Msg),
            ct:log("Unpacked Msg ~p", [Message]),
            get_replies(Socket, lists:delete(element(1, Message), Expected))
    end.
