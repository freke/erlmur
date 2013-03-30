%%%-------------------------------------------------------------------
%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 17 Mar 2013 by  <davabe@hotmail.com>
%%%-------------------------------------------------------------------
-module(erlmur_client).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 send/2,
	 send_udp/2, 
	 newkey/2, 
	 newuser/2, 
	 deluser/2,
	 cryptkey/1,
	 handle_msg/3,
	 session_id/1, 
	 session_id/2,
	 udp_tunnel/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(MAX_IDLE_MS,30000).

-record(state, {socket,cryptkey,udp_port,sid,use_udp_tunnle=true}).

%%%===================================================================
%%% API
%%%===================================================================
send(Pid,Data) ->
    gen_server:cast(Pid,{send, Data}).

send_udp(Pid,Data) ->
    gen_server:cast(Pid,{send_udp, Data}).

newuser(Pid,UserState) ->
    gen_server:cast(Pid,{userstate,UserState}).

deluser(Pid,UserState) ->
    gen_server:cast(Pid,{deluser,UserState}).

cryptkey(Pid) ->
    gen_server:call(Pid,cryptkey).

newkey(Pid,Key) ->
    gen_server:cast(Pid,{newkey,Key}).

handle_msg(Pid,PortNo,Msg) ->
    gen_server:cast(Pid,{handle_msg,PortNo,Msg}).

session_id(Pid) ->
    gen_server:call(Pid,sid).

session_id(Pid,Sid) ->
    gen_server:cast(Pid,{sid,Sid}).

udp_tunnel(Pid) ->
    gen_server:cast(Pid,udp_tunnel).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).


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
handle_call(cryptkey, _From, State=#state{cryptkey=Key}) ->
    {reply, ocb128crypt:key(Key), State};
handle_call(sid, _From, State=#state{sid=Sid}) ->
    {reply, Sid, State};
handle_call(_Request, _From, State) ->
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
handle_cast({socket,Socket}, State) ->
    ssl:controlling_process(Socket, self()),
    ok = ssl:ssl_accept(Socket),
    Key = ocb128crypt:new_key(),
    ssl:setopts(Socket, [{active, once}]),
    error_logger:info_msg("New connection~n"),
    {noreply, State#state{socket=Socket,cryptkey=Key}};

handle_cast({userstate,UR},State=#state{socket=Socket,cryptkey=Key}) ->
    {ok, {Address, Port}} = ssl:peername(Socket),
    erlmur_message:handle({userstate,UR},{self(),Key,{Address, Port}}),
    {noreply,State};

handle_cast({deluser,UR},State=#state{socket=Socket,cryptkey=Key}) ->
    {ok, {Address, Port}} = ssl:peername(Socket),
    erlmur_message:handle({deluser,UR},{self(),Key, {Address, Port}}),
    {noreply,State};

handle_cast({send,Data},#state{socket=Socket}=State) ->
    ssl:send(Socket, Data),
    {noreply, State};

handle_cast({send_udp,<<1:3,0:5,_/binary>>=Data},State=#state{socket=Socket,udp_port=Port,cryptkey=Key}) ->
    {ok, {Address, _}} = ssl:peername(Socket),
    {Msg,NewKey} = ocb128crypt:encrypt(Key,Data),
    erlmur_udp_server:send(Address,Port,Msg),
    {noreply, State#state{cryptkey=NewKey}};

handle_cast({send_udp,Data},State=#state{socket=Socket,udp_port=Port,cryptkey=Key,use_udp_tunnle=false}) ->
    {ok, {Address, _}} = ssl:peername(Socket),
    {Msg,NewKey} = ocb128crypt:encrypt(Key,Data),
    erlmur_udp_server:send(Address,Port,Msg),
    {noreply, State#state{cryptkey=NewKey}};

handle_cast({send_udp,Data},State=#state{socket=Socket,cryptkey=Key,use_udp_tunnle=true}) ->
    {ok, {Address, Port}} = ssl:peername(Socket),
    erlmur_message:handle({udp_tunnle,Data},{self(),Key,{Address,Port}}),
    {noreply, State};

handle_cast({handle_msg,PortNo,EncryptedMsg}, #state{cryptkey=Key,socket=Socket} = State) ->
    {ok, {Address, Port}} = ssl:peername(Socket),
    NewKey = case ocb128crypt:decrypt(Key, EncryptedMsg) of
		 {Msg,K} ->
		     erlmur_message:handle_udp(Msg, {self(), K, {Address, Port}}),
		     K;
		 error ->
		     error_logger:error_report([{erlmur_client,handle_msg},
						{port,Port},
						{msg,EncryptedMsg},
						{key,Key},
						{socket,Socket}]),
		     Key
	     end,
    {noreply,State#state{cryptkey=NewKey, udp_port=PortNo, use_udp_tunnle=false}};

handle_cast({newkey,Key}, State) ->
    {noreply,State#state{cryptkey=Key}};

handle_cast({sid,Sid}, State) ->
    error_logger:info_msg("New user ~w~n",[Sid]),
    {noreply,State#state{sid=Sid}};

handle_cast(udp_tunnel, State) ->
    {noreply,State#state{use_udp_tunnle=true}};

handle_cast(Msg, State) ->
    error_logger:info_report([{?MODULE,"Unhandled message"},{msg,Msg}]),
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
handle_info({ssl, From, Msg}, #state{socket=Socket,cryptkey=Key}=State) ->
    {ok, {Address, Port}} = ssl:peername(From),
    erlmur_message:handle(Msg, {self(), Key, {Address, Port}}),
    ssl:setopts(Socket, [{active,once}]),
    {noreply, State, ?MAX_IDLE_MS};

handle_info(timeout,State) ->
    error_logger:info_msg("Timeout! ~p seconds idle~n", ?MAX_IDLE_MS/1000),
    {stop, normal, State};

handle_info({ssl_closed, _S}, State = #state{sid=Sid}) ->
    error_logger:info_msg("~w Disconnected!~n", [Sid]),
    erlmur_server:deluser(),
    {stop, normal, State}.

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
