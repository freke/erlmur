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
    [{timetrap,{minutes,1}}].

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
    [authenticate_test_case,move_to_channel_test_case,voice_udp_tunnel_test_case].


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
authenticate_test_case() -> 
    [].

move_to_channel_test_case() ->
    [].

voice_udp_tunnel_test_case() ->
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
authenticate_test_case(_Config) ->
    Pid = start_erlmur_client(1),

    authenticate("User",Pid),
    get_user("User"),

    true = meck:validate(ssl),

    stop_erlmur_client(Pid).

move_to_channel_test_case(_Config) ->
    Pid = start_erlmur_client(1),

    meck:expect(ssl,send, fun(ssl_socket_1,_Msg) -> ok end ),

    authenticate("User",Pid),
    User = get_user("User"),

    channel("Channel 1",Pid),
    Channel = get_channel("Channel 1"),

    move_to_channel(User,Channel,Pid),
    

    true = check_user_in_channel(User,Channel),
    true = meck:validate(ssl),

    stop_erlmur_client(Pid).

voice_udp_tunnel_test_case(_Config) ->
    Pid1 = start_erlmur_client(1),
    Pid2 = start_erlmur_client(2),
    Pid3 = start_erlmur_client(3),

    authenticate("user1",Pid1),
    authenticate("user2",Pid2),
    authenticate("user3",Pid3),

    channel("Channel 1",Pid3),
    
    User3 = get_user("user3"),
    Channel = get_channel("Channel 1"),
    move_to_channel(User3,Channel,Pid3),

    true = check_user_in_channel(User3,Channel),

    Voice = <<0,84,178,223,247,218,21,152,157,133,210,99,32,119,236,248,92,66,214,42,
	      171,163,21,27,146,189,15,119,5,128,159,150,25,240,156,194,50,128,15,121,
	      21,178,147,133,162,155,146,85,84,164,169,39,16,50,223,247,218,21,152,157, 
	      133,210,99,32,119,236,248,92,66,214,42,171,163,21,27,146,189,15,119,5,128,
	      159,150,25,240,156,194,50,128,15,121,21,178,147,133,162,155,146,85,84,164,
	      169,39,16>>,

   ExpectedVoice =  <<0,1,0,0,0,105,0,1,84,178,223,247,218,21,152,157,133,210,99,32,119,
		      236,248,92,66,214,42,171,163,21,27,146,189,15,119,5,128,159,150,25,
		      240,156,194,50,128,15,121,21,178,147,133,162,155,146,85,84,164,169,
		      39,16,50,223,247,218,21,152,157,133,210,99,32,119,236,248,92,66,214,
		      42,171,163,21,27,146,189,15,119,5,128,159,150,25,240,156,194,50,128,
		      15,121,21,178,147,133,162,155,146,85,84,164,169,39,16>>,

    {Key,C,S} = erlmur_client:cryptkey(Pid1),
    {EVoice,_} = ocb128crypt:encrypt({Key,S,C,<<16#FF>>,{0,0,0,0},{0,0,0,0}}, Voice),
    
    meck:expect(ssl, send, fun(ssl_socket_2,ExpectedVoice) -> ok;
			      (_,_) -> erlang:error(unexpected_message) end ),

    erlmur_client:handle_msg(Pid1,port,EVoice),
    timer:sleep(500),
    true = meck:validate(ssl),    

    stop_erlmur_client(Pid1),
    stop_erlmur_client(Pid2),
    stop_erlmur_client(Pid3).

voice_test_case(_Config) ->
    meck:new(erlmur_udp_server),
    meck:expect(erlmur_udp_server,send,fun(Address,Port,Data) -> ok end),
    meck:expect(erlmur_udp_server,terminate,fun(_,_) -> ok end),
    true = meck:validate(erlmur_udp_server),
    ok.

%%--------------------------------------------------------------------
%% Help functions
%%--------------------------------------------------------------------
authenticate(UserName,Pid) ->
    AuthMsg = erlmur_message:pack({authenticate,[{username,UserName},
						 {password,""},
						 {tokens,""},
						 {celt_versions,[]},
						 {opus,false}]}),
    Pid ! {ssl, self(), AuthMsg}.

channel(ChannelName,Pid) ->
    ChanMsg = erlmur_message:pack({channelstate,[{name,ChannelName}]}),
    Pid ! {ssl, self(), ChanMsg}.

move_to_channel(User,Channel,Pid) ->
    UserMsg = erlmur_message:pack({userstate,[{session,erlmur_users:session(User)},
					      {user_id,erlmur_users:id(User)},
					      {channel_id,erlmur_channels:id(Channel)}]}),
    Pid ! {ssl, self(), UserMsg}.



start_erlmur() ->
    %% Setup ssl
    meck:expect(ssl, listen, fun(Port, Options) -> meck:passthrough([Port, Options]) end),
    meck:expect(ssl, transport_accept, fun(ListenSocket) -> meck:passthrough([ListenSocket]) end),
    meck:expect(ssl, close, fun(_Socket) -> ok end),

    ok = application:start(erlmur).

stop_erlmur() ->
    application:stop(erlmur).
    
start_erlmur_client(Id) ->
    Socket = list_to_atom(lists:flatten(io_lib:format("ssl_socket_~p",[Id]))),
    %% Start erlmur_client
    meck:expect(ssl, controlling_process, fun(_Socket,_Pid) -> ok end),
    meck:expect(ssl, ssl_accept, fun(_Socket) -> ok end),
    meck:expect(ssl, setopts, fun(_Socket, _Options) -> ok end),

    meck:expect(ssl, send, fun(_Socket,_Msg) -> ok end ),
    meck:expect(ssl, peername, fun(Socket) -> {ok,{address, port}} end),

    {ok, Child} = supervisor:start_child(erlmur_client_sup, []),
    gen_server:cast(Child, {socket, Socket}),
    Child.

stop_erlmur_client(Child) ->
    supervisor:terminate_child(erlmur_client_sup, Child).

encode_message(Type, Msg) when is_list(Msg) ->
    encode_message(Type, erlang:iolist_to_binary(Msg));

encode_message(Type, Msg) when is_binary(Msg) ->
    Len = byte_size(Msg),
    <<Type:16/unsigned-big-integer, Len:32/unsigned-big-integer,Msg/binary>>.

get_user(Name) ->
    case erlmur_users:find_user({name,Name}) of
    	[U] ->
    	    U;
    	[] ->
    	    ok = timer:sleep(100),
    	    get_user(Name)
    end.

user_removed(Name) ->
    case erlmur_users:find_user({name,Name}) of
    	[U] ->
	    ok = timer:sleep(100),
    	    user_removed(Name),
    	    U;
    	[] ->
    	    ok
    end.

get_channel(Name) ->
    case erlmur_channels:find_by_name(Name) of
    	[C] ->
    	    C;
    	[] ->
    	    ok = timer:sleep(100),
    	    get_channel(Name)
    end.

check_user_in_channel(User,Channel) ->
    ChannelId = erlmur_channels:id(Channel),
    case erlmur_users:find_user({channel_id,ChannelId}) of
	[] ->
	    ok = timer:sleep(100),
	    check_user_in_channel(User,Channel);
	Users ->
	    error_logger:info_report([{erlmur_SUIT,check_user_in_channel},{users,Users},{user,User}]),
	    lists:any(fun(U) -> erlmur_users:id(U) =:= erlmur_users:id(User) end, Users)
    end.

