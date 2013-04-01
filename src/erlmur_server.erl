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

%% API
-export([start_link/0,
	 version/0,
	 authenticate/3,
	 channelstates/0,
	 channelstate/1,
	 channel_remove/1,
	 userstates/0,
	 codecversion/0,
	 codecversion/1,
	 serverconfig/0,
	 serversync/1,
	 permissionquery/1,
	 deluser/0,
	 usercount/0,
	 client_session/1,
	 voice_data/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

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

-include("mumble_pb.hrl").

-record(state, {users,channels,cryptkeys,lastsid,clients}).

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

channel_remove(Channel) ->
    gen_server:cast(?SERVER,{channel_remove,Channel}).

userstates() ->
    gen_server:call(?SERVER,userstates).

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

deluser() ->
    gen_server:call(?SERVER,deluser).

usercount() ->
    gen_server:call(?SERVER,usercount).

client_session(Address) ->
    gen_server:call(?SERVER,{client_session,Address}).

voice_data(Type,Target,ClientPid,Counter,Voice,Positional) ->
    gen_server:cast(?SERVER,{voice_data,Type,Target,ClientPid,Counter,Voice,Positional}).

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
    C=erlmur_channels:init(),
    U=erlmur_users:init(),
    {ok, #state{users=U, channels=C, lastsid=0, clients=dict:new()}}.

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
    <<ErlmurVersion:4>> = <<0:2,0:1,1:1>>,
    {reply, #version{version = ErlmurVersion, 
		     release = <<"erlmur">>, 
		     os = list_to_binary(OsNameString),
		     os_version = list_to_binary(OsVersion)}, State};

handle_call({authenticate,User,Pass,Address}, {Pid,_}, #state{lastsid=Session,clients=C} = State) ->
    error_logger:info_report([{erlmur_server,handle_call},
			      {authenticate,User},
			      {pass,Pass},
			      {address,Address}]),
    NC=dict:store(Address,Pid,C),
    NewUsers = erlmur_users:add(Pid,User,Session+1,State#state.users),
    erlang:monitor(process, Pid),
    {reply, Session+1, State#state{users=NewUsers,lastsid=Session+1, clients=NC}};

handle_call(channelstates, 
	    _From, 
	    #state{channels=C}=State) ->
    {reply, C, State};

handle_call(userstates, 
	    _From, 
	    #state{users=U}=State) ->
    {reply, U, State};

handle_call(codecversion, 
	    _From, 
	    S) ->
    {reply, 
     #codecversion{alpha=-2147483637, 
		   beta=0, 
		   prefer_alpha=true, 
		   opus=false}, 
     S};

handle_call(serverconfig, _From, State) ->
    {reply, 
     #serverconfig{max_bandwidth=240000,
		   allow_html=true,
		   message_length=128}, 
     State};

handle_call({serversync,Session}, {_Pid,_}, State) ->
    {reply, 
     #serversync{session = Session,
		 max_bandwidth = 240000,
		 welcome_text = <<"Welcome to Erlmur.">>}, 
     State};

handle_call({permissionquery,Perm}, _From, S) ->
    {reply, Perm#permissionquery{permissions=?PERM_ALL}, S};

handle_call(deluser,{Pid,_}, State) ->
    NU = erlmur_users:remove(Pid,<<"Disconnected">>,State#state.users),
    {reply, ok, State#state{users=NU}};

handle_call(usercount, _From, State) ->
    NumUsers = erlmur_users:count(State#state.users),
    {reply, {NumUsers,10}, State};

handle_call({client_session,Address}, _From, #state{clients=C}=State) ->
    Pid = dict:find(Address,C),
    {reply,Pid,State};

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
    Sid = erlmur_varint:encode(erlmur_client:session_id(Pid)),
    C = erlmur_varint:encode(Counter),
    erlmur_client:send_udp(Pid,<<Type:3,0:5,Sid/binary,C/binary,Voice/binary>>),
    {noreply, State};

handle_cast({voice_data,Type,Target,Pid,_Counter,_Voice,_Positional},State) ->
    Sid = erlmur_client:session_id(Pid),
    error_logger:info_report([{erlmur_server,voice_data},{type,Type},{target,Target},{session_id,Sid}]),
    {noreply, State};

handle_cast({channelstate,ChannelState},State = #state{channels=C,users=U}) ->
    NewState = State#state{channels=erlmur_channels:update(ChannelState,U,C)},
    {noreply, NewState};

handle_cast({channel_remove,Channel},State = #state{channels=C,users=U}) ->
    NewState = State#state{channels=erlmur_channels:remove(Channel,U,C)},
    {noreply, NewState};

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
handle_info({'DOWN',_Ref,process,Pid,Reason}, State) when is_atom(Reason)->
    NU = erlmur_users:remove(Pid,atom_to_binary(Reason,latin1),State#state.users),
    {noreply, State#state{users=NU}};

handle_info({'DOWN',_Ref,process,Pid,Reason}, State) ->
    NU = erlmur_users:remove(Pid,list_to_binary(Reason),State#state.users),
    {noreply, State#state{users=NU}};

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

