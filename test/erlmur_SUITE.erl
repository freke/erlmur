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
    [authenticate_test_case,move_to_channel_test_case,voice_test_case].


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

voice_test_case() ->
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
    meck:new(ssl),

    start_erlmur(),
    Pid = start_erlmur_client(),
    
    Msg = erlmur_message:pack({authenticate,[{username,"User"},
					     {password,""},
					     {tokens,""},
					     {celt_versions,[]},
					     {opus,false}]}),
    Pid ! {ssl, self(), Msg},

    get_user("User"),

    true = meck:validate(ssl),

    stop_erlmur_client(Pid),
    stop_erlmur(),
    meck:unload(ssl).

move_to_channel_test_case(_Config) ->
    Pid = spawn_link(?MODULE, client_loop, [self()]),
    {ok,User} = authenticate_client(Pid,"user1"),

    {ok,Channel} = create_channel(Pid,"Channel 1"),
    ok = move_to_channel(Pid,User,Channel),
    true = check_user_in_channel(User,Channel),
    ok = stop_client(Pid),
    true = meck:validate(erlmur_client).

voice_test_case(_Config) ->
    Pid1 = spawn_link(?MODULE, client_loop, [self()]),
    Pid2 = spawn_link(?MODULE, client_loop, [self()]),
    Pid3 = spawn_link(?MODULE, client_loop, [self()]),

    Address = address,
    Port = 123,

    {ok,User1} = authenticate_client(Pid1,"user1"),
    {ok,User2} = authenticate_client(Pid2,"user2"),
    {ok,User3} = authenticate_client(Pid3,"user3"),

    {ok,Channel} = create_channel(Pid3,"Channel 1"),

    ok = move_to_channel(Pid3,User3,Channel),

    Pid = self(),
    Type = 0,
    Target = 0,
    VoiceData = crypto:rand_bytes(16),
    Voice = <<0,84,178,223,247,218,21,152,157,133,210,99,32,119,236,248,92,66,214,42,
	      171,163,21,27,146,189,15,119,5,128,159,150,25,240,156,194,50,128,15,121,
	      21,178,147,133,162,155,146,85,84,164,169,39,16,50,223,247,218,21,152,157, 
	      133,210,99,32,119,236,248,92,66,214,42,171,163,21,27,146,189,15,119,5,128,
	      159,150,25,240,156,194,50,128,15,121,21,178,147,133,162,155,146,85,84,164,
	      169,39,16>>,
    meck:expect(erlmur_client,send_udp, 
		fun(P,_) when P =:= Pid2 -> ok;
		   (_,_) -> erlang:error(should_not_recive_voice)
		end),
    speak(Pid1,Voice),
    timer:sleep(500),
    ok = stop_client(Pid1),
    ok = stop_client(Pid2),
    ok = stop_client(Pid3),
    true = meck:validate(erlmur_client).

%%--------------------------------------------------------------------
%% Help functions
%%--------------------------------------------------------------------
start_erlmur() ->
    %% Setup ssl
    meck:expect(ssl, listen, fun(Port, Options) -> meck:passthrough([Port, Options]) end),
    meck:expect(ssl, transport_accept, fun(ListenSocket) -> meck:passthrough([ListenSocket]) end),
    meck:expect(ssl, close, fun(_Socket) -> ok end),

    %% ssl mesage
    meck:expect(ssl, peername, fun(ssl_socket) -> {ok,{address, port}} end),
    meck:expect(ssl,send, fun(ssl_socket,_Msg) -> ok end ),

    ok = application:start(erlmur).

stop_erlmur() ->
    application:stop(erlmur).
    
start_erlmur_client() ->
    %% Start erlmur_client
    meck:expect(ssl, controlling_process, fun(ssl_socket,_Pid) -> ok end),
    meck:expect(ssl, ssl_accept, fun(ssl_socket) -> ok end),
    meck:expect(ssl, setopts, fun(ssl_socket, _Options) -> ok end),

    {ok, Child} = supervisor:start_child(erlmur_client_sup, []),
    gen_server:cast(Child, {socket, ssl_socket}),
    Child.

stop_erlmur_client(Child) ->
    supervisor:terminate_child(erlmur_client_sup, Child).

encode_message(Type, Msg) when is_list(Msg) ->
    encode_message(Type, erlang:iolist_to_binary(Msg));

encode_message(Type, Msg) when is_binary(Msg) ->
    Len = byte_size(Msg),
    <<Type:16/unsigned-big-integer, Len:32/unsigned-big-integer,Msg/binary>>.

send_msg(Type,Msg) ->
    BinMsg = encode_message(Type,Msg),
    Key = ocb128crypt:new_key(),
    erlmur_message:handle(BinMsg,{self(),Key,{address,port,cert}}).

client_loop(Parent) ->
    receive
	stop ->
	    error_logger:info_report([{erlmur_SUITE,client_loop},{stop,self()},{parent,Parent}]),
	    Parent ! {ok,stop};
	{authenticate,Name} ->
	    Pid = self(),
	    meck:expect(erlmur_client, send, fun(Pid,_) -> ok end),
	    Msg = mumble_pb:encode_authenticate(#authenticate{username=Name}),
	    send_msg(16#02,Msg),
	    client_loop(Parent);
	{kick_user,User,Reason} ->
	    SessionId = erlmur_users:session(User),
	    Msg = mumble_pb:encode_userremove(#userremove{session=SessionId,
							  reason=Reason,
							  ban = false}),
	    send_msg(16#08,Msg),
	    client_loop(Parent);
	{new_channel,Name} ->
	    Msg = mumble_pb:encode_channelstate(#channelstate{name=Name}),
	    send_msg(16#07,Msg),
	    client_loop(Parent);
	{move_user_to_channel,User,Channel} ->
	    SessionId = erlmur_users:session(User),
	    ChannelId = erlmur_channels:id(Channel),
	    Msg = mumble_pb:encode_userstate(#userstate{session=SessionId,channel_id=ChannelId}),
	    send_msg(16#09,Msg),
	    client_loop(Parent);
	{speak, Msg} ->
	    erlmur_message:handle_udp(Msg, {self(), key, {address, port}}),
	    client_loop(Parent);
	Unhandled ->
	    error_logger:info_report([{client_loop,unhandled_msg},
				      {self,self()},
				      {parent,Parent},
				      {msg,Unhandled}]),
	    client_loop(Parent)
    end.

stop_client(Pid) ->
    Pid ! stop,
    receive
	{ok,stop} ->
	    ok
    end.	

authenticate_client(Pid,Name) ->
    Pid ! {authenticate,Name},
    {ok,get_user(Name)}.

kick_user(Pid,User,Reason) ->
    Pid ! {kick_user,User,Reason},
    Name = erlmur_users:name(User),
    user_removed(Name),
    ok.

create_channel(Pid,Name) ->
    Pid ! {new_channel,Name},
    {ok,get_channel(Name)}.

move_to_channel(Pid,User,Channel) ->
    Pid ! {move_user_to_channel,User,Channel},
    ok.

speak(Pid, Data) ->
    Pid ! {speak, Data}.

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
    
