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
	 udp_tunnel/1,
	 update_key_remote/2, 
	 resync/2,
	 cryptkey/1,
	 handle_msg/3,
	 session_id/1, 
	 session_id/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(MAX_IDLE_MS,30000).

-record(state, {socket,cryptkey,udp_port,sid,use_udp_tunnel=true}).

%%%===================================================================
%%% API
%%%===================================================================
send(Pid,Data) ->
    gen_server:cast(Pid,{send, Data}).

send_udp(Pid,Data) ->
    gen_server:cast(Pid,{send_udp, Data}).

udp_tunnel(Pid) ->
    gen_server:cast(Pid,use_udp_tunnel).

cryptkey(Pid) ->
    gen_server:call(Pid,cryptkey).

update_key_remote(Pid,Remote) ->
    gen_server:cast(Pid,{update_key_remote,Remote}).

handle_msg(Pid,PortNo,Msg) ->
    gen_server:cast(Pid,{handle_msg,PortNo,Msg}).

session_id(Pid) ->
    gen_server:call(Pid,sid).

session_id(Pid,Sid) ->
    gen_server:cast(Pid,{sid,Sid}).

resync(Pid,ClientNonce) ->
    gen_server:cast(Pid,{resync,ClientNonce}).

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

handle_cast({send,Data},#state{socket=Socket}=State) ->
    ssl:send(Socket, Data),
    {noreply, State};

handle_cast({send_udp,<<1:3,0:5,_/binary>>=Data},State=#state{socket=Socket,udp_port=Port,cryptkey=Key}) ->
    {ok, {Address, _}} = ssl:peername(Socket),
    {Msg,NewKey} = ocb128crypt:encrypt(Key,Data),
    erlmur_udp_server:send(Address,Port,Msg),
    {noreply, State#state{cryptkey=NewKey}};

handle_cast({send_udp,Data},State=#state{socket=Socket,udp_port=Port,cryptkey=Key,use_udp_tunnel=false}) ->
    {ok, {Address, _}} = ssl:peername(Socket),
    {Msg,NewKey} = ocb128crypt:encrypt(Key,Data),
    erlmur_udp_server:send(Address,Port,Msg),
    {noreply, State#state{cryptkey=NewKey}};

handle_cast({send_udp,Data},State=#state{socket=Socket,cryptkey=Key,use_udp_tunnel=true}) ->
    {ok, {Address, Port}} = ssl:peername(Socket),
    erlmur_message:handle({udp_tunnle,Data},{self(),Key,{Address,Port}}),
    {noreply, State};

handle_cast({handle_msg,PortNo,EncryptedMsg}, #state{cryptkey=Key,socket=Socket} = State) ->
    {ok, {Address, Port}} = ssl:peername(Socket),
    NewState = case ocb128crypt:decrypt(Key, EncryptedMsg) of
		 {Msg,NewKey} ->
		     erlmur_message:handle_udp(Msg, {self(), NewKey, {Address, Port}}),
		     State#state{cryptkey=NewKey, udp_port=PortNo, use_udp_tunnel=false};
		 error ->
		     State
	     end,
    {noreply,NewState};

handle_cast({update_key_remote,{Good,Late,Lost,Resync}}, State = #state{cryptkey=Key}) ->
    NewKey = ocb128crypt:remote(Good,Late,Lost,Resync,Key),
    {noreply,State#state{cryptkey=NewKey}};

handle_cast({resync,ClientNonce}, State = #state{cryptkey=Key})->
    NewKey = ocb128crypt:client_nonce(ClientNonce,Key),
    {noreply,State#state{cryptkey=NewKey}};

handle_cast({sid,Sid}, State) ->
    error_logger:info_msg("New user ~w~n",[Sid]),
    {noreply,State#state{sid=Sid}};

handle_cast(use_udp_tunnel, State) ->
    {noreply,State#state{use_udp_tunnel=true}};

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
    {ok,Cert} = ssl:peercert(Socket),
    erlmur_message:handle(Msg, {self(), Key, {Address, Port, Cert}}),
    ssl:setopts(Socket, [{active,once}]),
    {noreply, State, ?MAX_IDLE_MS};

handle_info(timeout,State) ->
    error_logger:info_msg("Timeout! ~p seconds idle~n", ?MAX_IDLE_MS/1000),
    {stop, normal, State};

handle_info({ssl_closed, _S}, State = #state{sid=Sid}) ->
    error_logger:info_msg("~w Disconnected!~n", [Sid]),
    {stop, normal, State};

handle_info(Msg, State = #state{socket=Socket,cryptkey=Key}) ->
    {ok, {Address, Port}} = ssl:peername(Socket),
    erlmur_message:handle(Msg,{self(),Key,{Address, Port}}),
    {noreply,State}.

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
