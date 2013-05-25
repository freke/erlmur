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

-compile ({ parse_transform, lyet }).

%% API
-export([start_link/0,
	 send/2,
	 send_udp/2,
	 udp_tunnel/1,
	 update_key_remote/2, 
	 resync/2,
	 cryptkey/1,
	 handle_msg/3,
	 stop/1,
	 stats/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(MAX_IDLE_MS,30000).

-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {socket,cryptkey,udp_port,use_udp_tunnel=true,stats}).

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
    gen_server:call(Pid,get_cryptkey).

update_key_remote(Pid,Remote) ->
    gen_server:cast(Pid,{update_key_remote,Remote}).

handle_msg(Pid,PortNo,Msg) ->
    gen_server:cast(Pid,{handle_msg,PortNo,Msg}).

resync(Pid,ClientNonce) ->
    gen_server:cast(Pid,{resync,ClientNonce}).

stop(Pid) ->
    gen_server:cast(Pid,stop).

stats(Pid) ->
    gen_server:call(Pid,stats).

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
    {ok, #state{stats=erlmur_stats:new()}}.

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
handle_call(get_cryptkey, _From, State=#state{cryptkey=Key}) ->
    {reply, ocb128crypt:key(Key), State};
handle_call(stats, _From, State=#state{stats=Stats}) ->
    {reply, erlmur_stats:stats(Stats), State};
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
    Key = accept_new_connection(Socket),
    {noreply, State#state{socket=Socket,cryptkey=Key}};

handle_cast({send,Data},#state{socket=Socket}=State) ->
    ssl:send(Socket, Data),
    {noreply, State};

handle_cast({send_udp,Data},State=#state{socket=Socket,udp_port=Port,cryptkey=Key,use_udp_tunnel=false}) ->
    NewKey = send_with_udp(Data, Socket, Port, Key),
    {noreply, State#state{cryptkey=NewKey}};

handle_cast({send_udp,Data},State=#state{socket=Socket,use_udp_tunnel=true}) ->
    ssl:send(Socket,erlmur_message:pack({udp_tunnel,Data})),
    {noreply, State};

handle_cast({handle_msg,PortNo,EncryptedMsg}, #state{cryptkey=Key,socket=Socket,stats=Stats} = State) ->
    {ok, {Address, Port}} = ssl:peername(Socket),
    S = State#state{udp_port=PortNo},
    NewState = case ocb128crypt:decrypt(Key, EncryptedMsg) of
		   {Msg,NewKey} ->
		       NewS = maybe_toggle_udp_tunnel(S,false),
		       handle_udp_msg(erlmur_message:data_msg(Msg),NewS);
		   error ->
		       %error_logger:info_report([{erlmur_client,handle_cast},{handle_msg,"Error decrypting message"}]),
		       S
	       end,
    {noreply,NewState};

handle_cast({update_key_remote,{Good,Late,Lost,Resync}}, State = #state{stats=Stats}) ->
    NewStats = erlmur_stats:client_ping({Good,Late,Lost,Resync},Stats),
    {noreply,State#state{stats=NewStats}};

handle_cast({resync,ClientNonce}, State = #state{cryptkey=Key})->
    NewKey = ocb128crypt:client_nonce(ClientNonce,Key),
    {noreply,State#state{cryptkey=NewKey}};

handle_cast(use_udp_tunnel, State) ->
    {noreply,State#state{use_udp_tunnel=true}};

handle_cast(stop,State) ->
    {stop, normal, State};

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
handle_info({ssl, From, Msg}, #state{socket=Socket,stats=Stats} = State) ->
    Msgs = lists:map(fun({Type,Msg}) -> erlmur_message:unpack(Type,Msg) end, erlmur_message:control_msg(Msg)),
    NewState = handle_control_msg(Msgs,State),
    ssl:setopts(Socket, [{active,once}]),
    {noreply, NewState, ?MAX_IDLE_MS};

handle_info(timeout,State) ->
    error_logger:info_msg("Timeout! ~p seconds idle~n", ?MAX_IDLE_MS/1000),
    {stop, normal, State};

handle_info({ssl_closed, _S}, State) ->
    error_logger:info_msg("Disconnected!~n"),
    {stop, normal, State};

handle_info(Msg, #state{socket=Socket} = State) ->
    ssl:send(Socket,erlmur_message:pack(Msg)),
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
accept_new_connection(Socket) ->
    error_logger:info_report([{erlmur_client,accept_new_connection}]),
    ssl:controlling_process(Socket, self()),
    ok = ssl:ssl_accept(Socket),
    Key = ocb128crypt:new_key(),
    ssl:setopts(Socket, [{active, once}]),
    Key.

send_with_udp(Data, Socket, Port, Key) ->
    {ok, {Address, _}} = ssl:peername(Socket),
    {Msg,NewKey} = ocb128crypt:encrypt(Key,Data),
    erlmur_udp_server:send(Address,Port,Msg),
    NewKey.

handle_udp_msg(PingMsg, State=#state{socket=Socket,udp_port=Port,cryptkey=Key}) when is_binary(PingMsg) ->
    State#state{cryptkey=send_with_udp(PingMsg, Socket, Port, Key)};
handle_udp_msg({voice_data,Type,Target,Counter,Voice,Positional}, State) ->
    erlmur_server:voice_data(Type,Target,self(),Counter,Voice,Positional),
    State.

handle_control_msg([],State) ->
    State;
handle_control_msg([{version,Prop}|Rest],#state{socket=Socket}=State) ->
    R=erlmur_message:pack({version,erlmur_server:version()}),
    ssl:send(Socket,R),
    handle_control_msg(Rest,State);
handle_control_msg([{authenticate,Prop}|Rest],State=#state{socket=Socket,cryptkey=Key}) ->
    UserName = proplists:get_value(username,Prop),
    Password = proplists:get_value(password,Prop),
    CeltVersions = proplists:get_value(celt_versions,Prop),
    {ok, {Address, _Port}} = ssl:peername(Socket),
    Sid=erlmur_server:authenticate(UserName,Password,Address),
    send_all_channel_states(Socket),
    send_all_user_states(Socket),
    send_crypto_keys(Key, Socket),
    send_codecversion(CeltVersions, Socket),
    Config=erlmur_message:pack({serverconfig,erlmur_server:serverconfig()}),
    ssl:send(Socket,Config),
    ServerSync=erlmur_message:pack({serversync,erlmur_server:serversync(Sid)}),
    ssl:send(Socket,ServerSync),
    handle_control_msg(Rest,State);
handle_control_msg([{permissionquery,Prop}|Rest],State=#state{socket=Socket}) ->
    R=erlmur_message:pack({permissionquery,erlmur_server:permissionquery(Prop)}),
    ssl:send(Socket,R),
    handle_control_msg(Rest,State);
handle_control_msg([{userstate,Prop}|Rest],State) ->
    User = erlmur_users:fetch_user({client_pid,self()}),
    erlmur_users:update(Prop,User),
    handle_control_msg(Rest,State);
handle_control_msg([{userstats,Prop}|Rest],State=#state{socket=Socket,stats=Stats}) ->
    Session = proplists:get_value(session,Prop),
    Pid = self(),
    User = case erlmur_users:find_user({session,Session}) of
	       [] -> erlmur_users:fetch_user({client_pid,Pid});
	       [U] -> U
	   end,
    S = case erlmur_users:client_pid(User) of
	    Pid -> 
		erlmur_stats:stats(Stats);
	    C -> 
		stats(C)
	end,
    ssl:send(Socket,erlmur_message:pack({userstats,S})),
    handle_control_msg(Rest,State);
handle_control_msg([{userremove,Prop}|Rest],State) ->
    User = erlmur_users:fetch_user({client_pid,self()}),
    Remove = lists:keyreplace(actor, 1, Prop, {actor,erlmur_users:id(User)}),
    erlmur_server:userremove(Remove),
    handle_control_msg(Rest,State);
handle_control_msg([{channelstate,Prop}|Rest],State) ->
    erlmur_server:channelstate(Prop),
    handle_control_msg(Rest,State);
handle_control_msg([{channelremove,Prop}|Rest],State) ->
    erlmur_server:channelremove(Prop),
    handle_control_msg(Rest,State);
handle_control_msg([{ping,Prop}|Rest],State = #state{socket=Socket,stats=Stats}) ->
    NewStats = let_(Stats = erlmur_stats:client_ping(
			      {proplists:get_value(good,Prop),
			       proplists:get_value(late,Prop),
			       proplists:get_value(lost,Prop),
			       proplists:get_value(resync,Prop)},
			      Stats),
		    Stats = erlmur_stats:times({proplists:get_value(udp_ping_avg,Prop),
						proplists:get_value(udp_ping_var,Prop),
						proplists:get_value(tcp_ping_avg,Prop),
						proplists:get_value(tcp_ping_var,Prop)},
					       Stats),
		    erlmur_stats:packets({proplists:get_value(udp_packets,Prop),
					  proplists:get_value(tcp_packets,Prop)},
					 Stats)),
    
    {Good,Late,Lost,Resync} = erlmur_stats:server_ping(Stats),


    Pong = [{timestamp,proplists:get_value(timestamp,Prop)},
	    {good,Good},
	    {late,Late},
	    {lost,Lost},
	    {resync,Resync}],

    ssl:send(Socket,erlmur_message:pack({ping,Pong})),
    handle_control_msg(Rest,State#state{stats=NewStats});
handle_control_msg([{userlist,_Prop}|Rest], State=#state{socket=Socket}) ->
    Users = erlmur_users:list_registered_users(),
    ssl:send(Socket,erlmur_message:pack({userlist,Users})),
    handle_control_msg(Rest,State);
handle_control_msg([{banlist,Prop}|Rest], State=#state{socket=Socket}) ->
    case proplists:get_bool('query',Prop) of
        true ->
            Banlist = erlmur_server:list_banlist(),
            ssl:send(Socket,erlmur_message:pack({banlist,Banlist}));
        false ->
            ok
    end,
    handle_control_msg(Rest,State);
handle_control_msg([{udptunnel,Msg}|Rest], State=#state{socket=Socket}) ->
    ssl:send(Socket,erlmur_message:pack({udp_tunnel,Msg})),
    handle_control_msg(Rest,State);
handle_control_msg([{textmessage,Prop}|Rest], State=#state{socket=Socket}) ->
    User = erlmur_users:fetch_user({client_pid,self()}),
    Msg = lists:keyreplace(actor, 1, Prop, {actor,erlmur_users:id(User)}),

    DirectTo = erlmur_users:find_user({session,proplists:get_value(session,Prop)}),

    ParentChannels = erlmur_channels:find_by_id(proplists:get_value(tree_id,Prop)),
    SubChannels = erlmur_channels:subchannels(ParentChannels),
    Channels = erlmur_channels:find_by_id(proplists:get_value(channel_id,Prop)),

    Users = lists:delete(User,erlmur_users:users_in_channel(lists:append(Channels,SubChannels))),

    error_logger:info_report([{erlmur_client,textmessage},
			      {parentChannels,ParentChannels},
			      {subchannels,SubChannels},
			      {channels,Channels},
			      {users,Users},
			      {msg,Msg}]),
    
    send_to(lists:append(DirectTo,Users),{textmessage,Msg}),
    handle_control_msg(Rest,State).

send_codecversion(C, Socket) ->
    erlmur_server:codecversion(C),
    V=erlmur_message:pack({codecversion,erlmur_server:codecversion()}),
    ssl:send(Socket,V).

send_crypto_keys(Key, Socket) ->
    {K,DIV,EIV} = ocb128crypt:key(Key),
    CryptSetup = erlmur_message:pack({cryptsetup,[{key, K},
						  {client_nonce, DIV},
						  {server_nonce, EIV}]}),
    ssl:send(Socket,CryptSetup).

send_all_user_states(Socket) ->
    US=erlmur_users:all_user_states(),
    send_all(Socket,US).

send_all_channel_states(Socket) ->
    CS = erlmur_channels:all_channel_states(),
    error_logger:info_report([{erlmur_client,send_all_channels_states},
			      {channel_states,CS}]),
    send_all(Socket,CS).

send_all(Socket,Msg) ->
    lists:foreach(fun(K) ->
			  R=erlmur_message:pack(K),
			  ssl:send(Socket,R)
		  end, Msg).


maybe_toggle_udp_tunnel(State,UseUdpTunnel) ->
    UsingUdpTunnel = State#state.use_udp_tunnel,
    if
	UsingUdpTunnel =:= UseUdpTunnel -> 
	    State;
	UsingUdpTunnel =/= UseUdpTunnel ->
	    error_logger:info_report([{erlmur_client,maybe_toggle_udp_tunnel},
				      {using_udp_tunnel,UseUdpTunnel}]),
	    State#state{use_udp_tunnel=UseUdpTunnel}
    end.

send_to([],_) ->
    ok;
send_to([User|Users],Msg) ->
    send_to(User,Msg),
    send_to(Users,Msg);
send_to(User,Msg) ->
    erlmur_users:client_pid(User) ! Msg.
