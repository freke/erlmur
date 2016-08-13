%%%-------------------------------------------------------------------
%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013,
%%% @doc
%%%
%%% @end
%%% Created : 17 Mar 2013 by  <davabe@hotmail.com>
%%%-------------------------------------------------------------------
-module(erlmur_server).

-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("record_info/include/record_info.hrl").

%% API
-export(
	[
		start_link/0,
		version/0,
		authenticate/3,
		channelstates/0,
		channelstate/1,
		userstates/0,
		userstate/1,
		userremove/1,
		codecversion/0,
		codecversion/1,
		serversync/1,
		serverconfig/0,
		usercount/0,
		permissionquery/1,
		voice_data/6,
		list_banlist/0
	]).

%% gen_server callbacks
-export(
	[
		init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3
	]).

-define(PERM_NONE, 16#0).
-define(PERM_WRITE, 16#1).
-define(PERM_TRAVERSE, 16#2).
-define(PERM_ENTER, 16#4).
-define(PERM_SPEAK, 16#8).
-define(PERM_MUTEDEAFEN, 16#10).
-define(PERM_MOVE, 16#20).
-define(PERM_MAKECHANNEL, 16#40).
-define(PERM_LINKCHANNEL, 16#80).
-define(PERM_WHISPER, 16#100).
-define(PERM_TEXTMESSAGE, 16#200).
-define(PERM_MAKETEMPCHANNEL, 16#400).
% Root channel only
-define(PERM_KICK, 16#10000).
-define(PERM_BAN, 16#20000).
-define(PERM_REGISTER, 16#40000).
-define(PERM_SELFREGISTER, 16#80000).
-define(PERM_CACHED, 16#8000000).
-define(PERM_ALL, 16#f07ff).

-define(SERVER, ?MODULE).

-record(state, {}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

version() ->
    gen_server:call(?SERVER,version).

authenticate(User,Password,Address) ->
    gen_server:call(?SERVER,{authenticate, User, Password,Address}).

channelstates() ->
    gen_server:call(?SERVER,channelstates).

channelstate(ChannelState) ->
    gen_server:cast(?SERVER,{channelstate,ChannelState}).

userstates() ->
    gen_server:call(?SERVER,userstates).

userstate(UserState) ->
    gen_server:cast(?SERVER,{userstate,UserState}).

userremove(UserRemove) ->
    gen_server:cast(?SERVER,{userremove,UserRemove}).

codecversion() ->
    gen_server:call(?SERVER,codecversion).

codecversion(C) ->
    gen_server:cast(?SERVER,{codecversion,C}).

serverconfig() ->
    gen_server:call(?SERVER,serverconfig).

serversync(Session) ->
    gen_server:call(?SERVER,{serversync,Session}).

permissionquery(Perm) ->
    gen_server:call(?SERVER,{permissionquery,Perm}).

usercount() ->
    gen_server:call(?SERVER,usercount).

voice_data(Type,Target,ClientPid,Counter,Voice,Positional) ->
    gen_server:cast(?SERVER,{voice_data,Type,Target,ClientPid,Counter,Voice,Positional}).

list_banlist() ->
    [].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  Nodes = [node()],

	ChannelTables = erlmur_channels:init(Nodes),
	UserTables = erlmur_users:init(Nodes),

  erlmur_channel_feed:start_link(),

  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(version, _From, State) ->
  {OsFamily, OsName} = os:type(),
  OsNameString = io_lib:format("~w-~w",[OsFamily,OsName]),
  OsVersion = case os:version() of
		{Major, Minor, Release} ->
			io_lib:format("~w.~w.~w",[Major,Minor,Release]);
		V -> V
	end,
	<<ErlmurVersion:32>> = <<1:16,2:8,3:8>>,
	{
		reply,
		[
			{version, ErlmurVersion},
			{release, <<"erlmur">>},
			{os, list_to_binary(OsNameString)},
			{os_version, list_to_binary(OsVersion)}
		],
		State
	};

handle_call({authenticate,User,Pass,Address}, {Pid,_}, State) ->
  error_logger:info_report([{erlmur_server,handle_call},
		      {authenticate,User},
		      {pass,Pass},
		      {address,Address}]),
  Session = erlmur_users:add(Pid,User,Address),
  erlmur_monitor_users:monitor_user(Pid),
  {reply, Session, State};

handle_call(channelstates, _From, State) ->
	ChannelStates = erlmur_channels:channelstates(),
  {reply, ChannelStates, State};

handle_call(userstates, {Pid,_}, State) ->
	User = erlmur_users:find_user({client_pid,Pid}),
  {reply, erlmur_users:all_user_states(User), State};

handle_call(codecversion, _From, State) ->
    {reply,
     [{alpha, -2147483637},
      {beta, 0},
      {prefer_alpha, true},
      {opus, false}],
     State};

handle_call(serverconfig, _From, State) ->
    {reply,
     [{max_bandwidth, 240000},
      {allow_html, true},
      {message_length, 128}],
     State};

handle_call({serversync,Session}, {_Pid,_}, State) ->
    {reply,
     [{session, Session},
      {max_bandwidth, 240000},
      {welcome_text, <<"Welcome to Erlmur.">>}],
     State};

handle_call({permissionquery,Perm}, {Pid,_}, State) ->
  NewPerm = case proplists:get_value(channel_id,Perm) of
	  undefined -> Perm;
	  ChannelId ->
	      Channel = erlmur_channels:find({channel_id, ChannelId}),
	      User = erlmur_users:find_user({client_pid,Pid}),
	      Permissions = permissions(Channel,User),
	      lists:keyreplace(permissions, 1, Perm, {permissions,Permissions})
      end,
  {reply, NewPerm, State};

handle_call(usercount, _From, State) ->
    NumUsers = proplists:get_value(workers,supervisor:count_children(erlmur_client_sup)),
    {reply, {NumUsers,10}, State};

handle_call({channel_filter,Filter}, _From, State) ->
	FilterdChannels = erlmur_channels:filter(Filter),
  {reply, FilterdChannels, State};

handle_call(Request, _From, State) ->
    error_logger:info_report([{erlmur_server,handle_call},{unhandled_request,Request}]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({codecversion,_C}, State) ->
    {noreply, State};

handle_cast({voice_data,Type,16#1F,Pid,Counter,Voice,_Positional},State) ->
  User = erlmur_users:fetch_user({client_pid,Pid}),
  Sid = erlmur_users:session(User),
  C = erlmur_varint:encode(Counter),
  EncodedSid = erlmur_varint:encode(Sid),
  erlmur_client:send_udp(Pid,<<Type:3,0:5,EncodedSid/binary,C/binary,Voice/binary>>),
  {noreply, State};

handle_cast({voice_data,Type,16#00,Pid,Counter,Voice,Positional},State) ->
    User = erlmur_users:fetch_user({client_pid,Pid}),
    Sid = erlmur_users:session(User),
    ChannelId = erlmur_users:channel_id(User),
		LinkedChannels = erlmur_channels:linked(ChannelId),
    Users = lists:filter(fun(U) -> erlmur_users:session(U) =/= Sid end, erlmur_users:find_user({channel_id,ChannelId})),
    C = erlmur_varint:encode(Counter),
    EncodedSid = erlmur_varint:encode(Sid),
    error_logger:info_report([{erlmur_server,voice_data},{users,Users}]),
    lists:foreach(fun(U) ->
			  P = erlmur_users:client_pid(U),
			  erlmur_client:send_udp(P,
						 <<Type:3,
						   0:5,
						   EncodedSid/binary,
						   C/binary,
						   Voice/binary,
						   Positional/binary>>)
		  end, Users),
    {noreply, State};

handle_cast({channelstate,PropList}, State) ->
	erlmur_channels:channelstate(PropList),
  {noreply, State};

handle_cast({channelremove,PropList, _Actor}, State) ->
	Channel = erlmur_channels:find({channel_id, proplists:get_value(channel_id,PropList)}),
	erlmur_channels:remove(Channel),
  {noreply, State};

handle_cast({userremove,UserRemove}, State) ->
    error_logger:info_report([{erlmur_server,handle_cast},{userremove,UserRemove}]),
    Users = erlmur_users:find_user({session,proplists:get_value(session,UserRemove)}),
    lists:foreach(fun(U) ->
			 erlmur_users:remove(U,
					     proplists:get_value(actor,UserRemove),
					     proplists:get_value(reason,UserRemove),
					     proplists:get_value(ban,UserRemove)),
			 erlmur_client:stop(erlmur_users:client_pid(U))
		 end, Users),
    {noreply, State};

handle_cast(Msg, State) ->
    error_logger:info_report([{erlmur_server,handle_cast},{"Unhandle msg",Msg}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    error_logger:info_report([{erlmur_server,handle_info},{"Unhandle info",Info}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% Stream of integers %%%
permissions(_Channel,_User) ->
  ?PERM_ALL. %?PERM_NONE.
