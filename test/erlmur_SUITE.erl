%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2013 by  <>
%%%-------------------------------------------------------------------
-module(erlmur_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("mumble_pb.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%  Returns list of tuples to set default properties
%%  for the suite.
%%
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%
%% @spec suite() -> Info
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,15}}].

%%--------------------------------------------------------------------
%% @doc
%% Initialization before the whole suite
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    ServerPem = filename:join([DataDir, "server.pem"]),
    KeyPem = filename:join([DataDir, "key.pem"]),
    application:set_env(erlmur, server_pem, ServerPem),
    application:set_env(erlmur, key_pem, KeyPem),
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(mnesia),
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after the whole suite
%%
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    application:stop(mnesia),
    application:stop(ssl),
    application:stop(public_key),
    application:stop(crypto),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case group.
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case group.
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case
%%
%% TestCase - atom()
%%   Name of the test case that is about to be run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    meck:new(ssl),
    start_erlmur(),
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case
%%
%% TestCase - atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    stop_erlmur(),
    meck:unload(ssl),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of test case group definitions.
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% @spec: groups() -> [Group]
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @doc
%%  Returns the list of groups and test cases that
%%  are to be executed.
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping all groups and test cases.
%%
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% @end
%%--------------------------------------------------------------------
all() -> 
    [version_msg_test_case, 
     authenticate_msg_test_case, 
     ping_msg_test_case,
     permissionquery_msg_test_case,
     userstate_msg_test_case,
     userstats_msg_test_case,
     userremove_msg_test_case,
     channelstate_msg_test_case].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc 
%%  Test case info function - returns list of tuples to set
%%  properties for the test case.
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Note: This function is only meant to be used to return a list of
%% values, not perform any other operations.
%%
%% @spec TestCase() -> Info 
%% @end
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

%%--------------------------------------------------------------------
%% @doc Test case function. (The name of it must be specified in
%%              the all/0 list or in a test case group for the test case
%%              to be executed).
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% @spec TestCase(Config0) ->
%%           ok | exit() | {skip,Reason} | {comment,Comment} |
%%           {save_config,Config1} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
version_msg_test_case(_Config) ->
    Client = start_erlmur_client(),
    send_version(Client).

authenticate_msg_test_case(_Config) ->
    Client = start_erlmur_client(),
    send_authenticate(Client).

ping_msg_test_case(_Config) ->
    Client = start_erlmur_client(),
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
    send_userremove(Client2,Client1).

channelstate_msg_test_case(_Config) ->
    Client = start_erlmur_client(),
    send_authenticate(Client),
    send_new_channel(Client,[{parent,0},{name,"Test"}]),
    send_remove_channel(Client,1).

%%--------------------------------------------------------------------
%% Help functions
%%--------------------------------------------------------------------
start_erlmur() ->
    %% Setup ssl
    meck:expect(ssl, listen, fun(Port, Options) -> meck:passthrough([Port, Options]) end),
    meck:expect(ssl, transport_accept, fun(ListenSocket) -> meck:passthrough([ListenSocket]) end),
    meck:expect(ssl, close, fun(_Socket) -> ok end),
    meck:expect(ssl, controlling_process, fun(_Socket,_Pid) -> ok end),
    meck:expect(ssl, ssl_accept, fun(_Socket) -> ok end),
    meck:expect(ssl, setopts, fun(_Socket,_Options) -> ok end),
    meck:expect(ssl, peername, fun(Socket) ->{ok, {Socket, port}} end),

    Server = self(),

    meck:expect(ssl, send, fun(Socket,Msg) -> Server ! {Socket,Msg} end),

    ok = application:start(erlmur).

stop_erlmur() ->
    application:stop(erlmur).

start_erlmur_client() ->
    Socket = make_ref(),
    {ok, Pid} = supervisor:start_child(erlmur_client_sup, []),
    gen_server:cast(Pid, {socket, Socket}),
    {Pid,Socket}.

stop_erlmur_client({Pid,_}) ->
    supervisor:terminate_child(erlmur_client_sup, Pid).

send_version({Pid,Socket}) ->
    VersionMsg = erlmur_message:pack({version,[]}),
    Pid ! {ssl, self(), VersionMsg},
    get_replies(Socket,[version]).

send_authenticate({Pid,Socket}) ->
    AuthenticateMsg = erlmur_message:pack({authenticate,[]}),
    Pid ! {ssl, self(), AuthenticateMsg},
    get_replies(Socket,[channelstate,userstate,cryptsetup,codecversion,serverconfig,serversync]).

send_ping({Pid,Socket}) ->
    PingMsg = erlmur_message:pack({ping,[]}),
    Pid ! {ssl, self(), PingMsg},
    get_replies(Socket,[ping]).

send_permissionquery({Pid,Socket}) ->
    PermissionqueryMsg = erlmur_message:pack({permissionquery,[]}),
    Pid ! {ssl, self(), PermissionqueryMsg},
    get_replies(Socket,[permissionquery]).

send_userstate({Pid,Socket}) ->
    UserStateMsg = erlmur_message:pack({userstate,[]}),
    Pid ! {ssl, self(), UserStateMsg},
    get_replies(Socket,[userstate]).

send_userstats({Pid,Socket}) ->
    UserStatsMsg = erlmur_message:pack({userstats,[]}),
    Pid ! {ssl, self(), UserStatsMsg},
    get_replies(Socket,[userstats]).

send_userremove({Pid,Socket},{PidToRemove,_}) ->
    [UserToRemove] = erlmur_users:find_user({client_pid,PidToRemove}),
    UserRemoveMsg = erlmur_message:pack({userremove,[{session,erlmur_users:session(UserToRemove)}]}),
    Pid ! {ssl, self(), UserRemoveMsg},
    get_replies(Socket,[userremove]).

send_new_channel({Pid,Socket},NewChannel) ->
    ChannelStateMsg = erlmur_message:pack({channelstate,NewChannel}),
    Pid ! {ssl, self(), ChannelStateMsg},
    get_replies(Socket,[channelstate]).

send_remove_channel({Pid,Socket},ChannelId) ->
    ChannelRemoveMsg = erlmur_message:pack({channelremove,[{channel_id,ChannelId}]}),
    Pid ! {ssl, self(), ChannelRemoveMsg},
    get_replies(Socket,[channelremove]).

get_replies(_Socket,[]) ->
    ok;
get_replies(Socket,Expected) ->
    receive
	{Socket,Msg} ->
	    [{Type,M}] = erlmur_message:control_msg(Msg),
	    {T,_} = erlmur_message:unpack(Type,M),
	    get_replies(Socket,lists:delete(T,Expected))
    end.
