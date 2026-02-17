-module(mumble_server_conn).

-moduledoc """
Server-side connection handler for the Mumble protocol.

This module implements both `gen_statem` and `ranch_protocol` behaviours to
handle incoming client connections to a Mumble server. It manages the full
connection lifecycle including authentication, encryption setup, and voice/data
exchange.

Each connection spawns a separate process that:
1. Performs TLS handshake via Ranch
2. Authenticates the client
3. Sets up OCB-AES128 encryption
4. Handles voice (UDP/TCP) and control messages
5. Manages connection statistics

The connection progresses through these states:
- `authenticating` - Protocol handshake and user authentication
- `established` - Active connection handling voice and control traffic
""".

-behaviour(gen_statem).
-behaviour(ranch_protocol).

-include("mumble_protocol.hrl").
-include("Mumble_gpb.hrl").
-include_lib("ocb128_crypto/include/ocb128_crypto.hrl").

%% API
-export([start_link/3]).
-export([send/2, send_udp/2, get_state/1, voice_data/2, subscribe_stats/3]).
-export([udp_packet/3]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([authenticating/3, established/3]).

-define(TIMEOUT, 60000).

-record(state, {
    ref,
    transport,
    socket,
    handler_mod,
    handler_state,
    session_id,
    channel_id = 0,
    crypto_state,
    stats = #stats{},
    mumble_protocol = v1_2,
    udp_verified = false,
    udp_timer,
    udp_addr,
    stats_events
}).

-define(UDP_TIMEOUT, 10000).

%%%===================================================================
%%% API
%%%===================================================================

-doc """
Start a Mumble server connection process for handling a client.
Input: Ranch reference, transport module, and options list.
Output: {ok, pid()} on success, {error, term()} on failure.
""".
-spec start_link(ranch:ref(), module(), list()) -> {ok, pid()} | {error, term()}.
start_link(Ref, Transport, Opts) ->
    gen_statem:start_link(?MODULE, {Ref, Transport, Opts}, []).

-doc """
Send a message to the connected client.
Input: Connection PID and message map.
Output: ok (message sent asynchronously).
""".
-spec send(pid(), map()) -> ok.
send(Pid, Msg) ->
    gen_statem:cast(Pid, {send, Msg}).

-doc """
Send an encrypted UDP message to the connected client.
Input: Connection PID and encrypted message binary.
Output: ok (message sent asynchronously).
""".
-spec send_udp(pid(), binary()) -> ok.
send_udp(Pid, Msg) ->
    gen_statem:cast(Pid, {send_udp, Msg}).

-doc """
Send voice data to the connected client.
Input: Connection PID and voice data tuple {voice_data, Type, SenderSession, Target, Counter, Voice, Positional}.
  - Type: Codec type (0=CELT Alpha, 2=Speex, 3=CELT Beta, 4=Opus)
  - SenderSession: Session ID of the original sender (required for server->client)
  - Target: Target ID (0=normal, 31=loopback, 1-30=whisper targets)
  - Counter: Sequence number
  - Voice: Voice data binary
  - Positional: Optional positional data
Output: ok (voice data sent asynchronously).
""".
-spec voice_data(pid(), {voice_data, non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer(), binary(), any()}) -> ok.
voice_data(Pid, Msg) ->
    gen_statem:cast(Pid, {voice_data, Msg}).

-doc """
Get the current state of the server connection.
Input: Connection PID.
Output: Connection state record containing session information.
""".
-spec get_state(pid()) -> #state{}.
get_state(Pid) ->
    gen_statem:call(Pid, get_state).

-doc """
Subscribe to connection statistics events.
Input: Connection PID, handler module, and handler arguments.
Output: {ok, pid()} on success, {error, term()} on failure.
""".
-spec subscribe_stats(pid(), module(), list()) -> {ok, pid()} | {error, term()}.
subscribe_stats(Pid, Handler, Args) ->
    gen_statem:call(Pid, {subscribe_stats, Handler, Args}).

-doc """
Handle an incoming UDP packet for this connection.
Input: Connection PID, packet binary, and client address tuple.
Output: ok (packet handled asynchronously).
""".
-spec udp_packet(pid(), binary(), {inet:ip_address(), inet:port_number()}) -> ok.
udp_packet(Pid, Data, Addr) ->
    gen_statem:cast(Pid, {udp_packet, Data, Addr}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() ->
    [state_functions, state_enter].

init({Ref, Transport, [HandlerMod | HandlerOpts]}) ->
    {ok, HandlerState} = HandlerMod:init(HandlerOpts),
    {ok, authenticating,
    #state{
        ref = Ref,
        transport = Transport,
        handler_mod = HandlerMod,
        handler_state = HandlerState,
        crypto_state = ocb128_crypto:init(),
        stats_events = start_stats_manager()
    },
    ?TIMEOUT}.

terminate(Reason, StateName, StateData = #state{socket = Socket, transport = Transport, stats_events = Events}) when
    Socket =/= undefined, Transport =/= undefined
->
    Transport:close(Socket),
    stop_stats_manager(Events),
    terminate(
        Reason, StateName, StateData#state{socket = undefined, transport = undefined}
    );
terminate(Reason, _StateName, _StateData) ->
    logger:debug("Connection terminated: ~p", [Reason]),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%%===================================================================
%%% State Functions
%%%===================================================================

authenticating(
    enter, _OldState, StateData = #state{ref = Ref, transport = Transport}
) ->
    {ok, Socket} = ranch:handshake(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    {keep_state, StateData#state{socket = Socket}};
authenticating(
    info, {ssl, Socket, Data}, StateData
) ->
    handle_tcp_data(Socket, Data, StateData, ['Version', 'Authenticate', 'Ping']);
authenticating(
    cast, {send, Msg}, StateData
) ->
    send_msg(StateData, Msg),
    {keep_state_and_data, ?TIMEOUT};
authenticating(
    cast, {voice_data, _Msg}, _StateData
) ->
    {keep_state_and_data, ?TIMEOUT};
authenticating(info, {ssl_closed, _Socket}, _StateData) ->
    {stop, normal};
authenticating({call, From}, {subscribe_stats, Handler, Args}, #state{stats_events = Manager}) ->
    Res = gen_event:add_sup_handler(Manager, Handler, Args),
    {keep_state_and_data, {reply, From, Res}};
authenticating({call, From}, get_state, StateData) ->
    {keep_state_and_data, {reply, From, StateData}};
authenticating(Type, Msg, StateData) ->
    logger:warning("State: authenticating~nUnhandled ~p~n~p~n~p", [
        Type, Msg, StateData
    ]),
    {stop, unhandled}.

established(enter, _OldState, _StateData) ->
    {keep_state_and_data, ?TIMEOUT};
established(cast, {send, Msg}, StateData) ->
    send_msg(StateData, Msg),
    {keep_state_and_data, ?TIMEOUT};
established(
    cast, {send_udp, Msg}, StateData = #state{crypto_state = CryptoState, udp_addr = UdpAddr}
) ->
    {ok, EncryptedMsg, NewCryptoState} = ocb128_crypto:encrypt(Msg, CryptoState),
    case UdpAddr of
        {IP, Port} ->
            mumble_udp_server:send(IP, Port, EncryptedMsg);
        undefined ->
            logger:debug("UDP addr not set, cannot send UDP packet")
    end,
    {keep_state, StateData#state{crypto_state = NewCryptoState}, ?TIMEOUT};
established(
    cast, {voice_data, Msg}, StateData
) ->
    {voice_data, _Type, SenderSession, _Target, _Counter, _Voice, _Positional} = Msg,
    case SenderSession == StateData#state.session_id of
        true ->
            %% This is our own voice, check Target and broadcast if needed
            send_voice(StateData, Msg);
        false ->
            %% This is a broadcast from another user, just send to our client
            do_send_voice_to_client(StateData, Msg)
    end,
    {keep_state_and_data, ?TIMEOUT};
established(
    cast, {udp_stats, {Bytes, CryptoStats}}, StateData = #state{stats = Stats, udp_timer = OldTimer}
) ->
    %% UDP message received and decrypted, so UDP is verified
    case OldTimer of
        undefined -> ok;
        _ -> erlang:cancel_timer(OldTimer)
    end,
    NewTimer = erlang:start_timer(?UDP_TIMEOUT, self(), udp_timeout),
    NewStats = update_connection_stats(Stats, CryptoStats, Bytes),
    notify_stats(StateData#state.stats_events, NewStats),
    
    HState = StateData#state.handler_state,
    {ok, NewHState} = case StateData#state.udp_verified of
        false -> 
            notify_status(StateData#state.handler_mod, udp_verified, StateData#state.session_id, HState);
        true -> {ok, HState}
    end,
    
    {keep_state, StateData#state{stats = NewStats, udp_verified = true, udp_timer = NewTimer, handler_state = NewHState}, ?TIMEOUT};
established(
    cast, {udp_packet, Data, Addr}, StateData = #state{crypto_state = CryptoState, session_id = SessionId, mumble_protocol = Protocol}
) ->
    case ocb128_crypto:decrypt(Data, CryptoState) of
        {ok, Decrypted, NewCryptoState} ->
            %% Register UDP addr if not already set
            NewStateData = case StateData#state.udp_addr of
                undefined ->
                    erlmur_user_manager:register_udp_addr(SessionId, Addr),
                    StateData#state{crypto_state = NewCryptoState, udp_addr = Addr};
                _ ->
                    StateData#state{crypto_state = NewCryptoState}
            end,
            %% Notify successful UDP receipt
            CryptoStats = ocb128_crypto:stats(NewCryptoState),
            gen_statem:cast(self(), {udp_stats, {byte_size(Data), CryptoStats}}),
            %% Process UDP data
            Session = #session{
                id = SessionId,
                session_pid = self(),
                mumble_protocol = Protocol
            },
            mumble_udp_proto:handle(Session, Decrypted),
            {keep_state, NewStateData, ?TIMEOUT};
        {error, Reason, NewCryptoState} ->
            logger:debug("UDP decrypt failed: ~p", [Reason]),
            {keep_state, StateData#state{crypto_state = NewCryptoState}, ?TIMEOUT}
    end;
established(info, {timeout, _Ref, udp_timeout}, StateData) ->
    logger:notice("UDP connection lost, falling back to TCP"),
    {ok, NewHState} = notify_status(StateData#state.handler_mod, udp_lost, StateData#state.session_id, StateData#state.handler_state),
    {keep_state, StateData#state{udp_verified = false, udp_timer = undefined, handler_state = NewHState}, ?TIMEOUT};
established(info, {ssl, Socket, Data}, StateData) ->
    handle_tcp_data(Socket, Data, StateData, all);
established(info, {ssl_closed, _Socket}, _StateData) ->
    {stop, normal};
established(info, {ssl_error, _Socket, Reason}, _StateData) ->
    {stop, {ssl_error, Reason}};
established(timeout, _Msg, _StateData) ->
    logger:warning("Timeout"),
    {stop, normal};
established({call, From}, {subscribe_stats, Handler, Args}, #state{stats_events = Manager}) ->
    Res = gen_event:add_sup_handler(Manager, Handler, Args),
    {keep_state_and_data, {reply, From, Res}};
established({call, From}, get_state, StateData) ->
    {keep_state_and_data, {reply, From, StateData}};
established(Type, Msg, StateData) ->
    logger:warning("State: established~nUnhandled ~p~n~p~n~p", [Type, Msg, StateData]),
    {keep_state_and_data, ?TIMEOUT}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_stats_manager() ->
    {ok, Pid} = gen_event:start_link(),
    Pid.

stop_stats_manager(Pid) ->
    gen_event:stop(Pid).

notify_stats(Manager, Stats) ->
    gen_event:notify(Manager, {mumble_stats, Stats}).

handle_tcp_data(Socket, Data, StateData = #state{transport = Transport, stats = Stats}, Allowed) when
    byte_size(Data) >= 1
->
    ok = Transport:setopts(Socket, [{active, once}]),
    NewStats = Stats#stats{
        from_client_tcp_packets = Stats#stats.from_client_tcp_packets + 1,
        from_client_tcp_bytes = Stats#stats.from_client_tcp_bytes + byte_size(Data)
    },
    DecodedMessages = mumble_tcp_proto:decode(Data),
    process_messages(DecodedMessages, Allowed, StateData#state{stats = NewStats}).

process_messages([], _Allowed, StateData) ->
    {keep_state, StateData, ?TIMEOUT};
process_messages([Msg | Rest], Allowed, StateData) ->
    MsgName = maps:get(message_type, Msg),
    IsAllowed = Allowed == all orelse lists:member(MsgName, Allowed),
    if
        IsAllowed ->
            case handle_protocol_msg(Msg, StateData) of
                {ok, NewStateData} ->
                    process_messages(Rest, Allowed, NewStateData);
                {transition, NextState, NewStateData} ->
                    process_messages_in_new_state(Rest, NextState, NewStateData)
            end;
        true ->
            logger:warning("Message ~p not allowed", [MsgName]),
            process_messages(Rest, Allowed, StateData)
    end.

process_messages_in_new_state([], NextState, NewStateData) ->
    {next_state, NextState, NewStateData, ?TIMEOUT};
process_messages_in_new_state(Rest, NextState, NewStateData) ->
    case NextState of
        established ->
             process_messages(Rest, all, NewStateData)
    end.

handle_protocol_msg(#{message_type := 'Version'}, StateData = #state{handler_mod = Mod}) ->
    Caps = case erlang:function_exported(Mod, get_caps, 1) of
        true -> Mod:get_caps(StateData#state.handler_state);
        false -> #{}
    end,
    {V1, V2} = version_enc(Caps),
    ServerVersion = #{
        message_type => 'Version',
        version_v1 => V1,
        version_v2 => V2,
        os => maps:get(os, Caps, ~"Linux"),
        release => maps:get(release, Caps, ~"1.2.4"),
        os_version => maps:get(os_version, Caps, ~"1.0")
    },
    send_msg(StateData, ServerVersion),
    {ok, StateData};
handle_protocol_msg(#{message_type := 'Authenticate'} = Msg, StateData = #state{handler_mod = Mod, handler_state = HState, crypto_state = CState}) ->
    case Mod:authenticate(Msg, HState) of
        {ok, UserInfo, NewHState} ->
            CryptSetup = #{
                message_type => 'CryptSetup',
                key => ocb128_crypto:key(CState),
                server_nonce => ocb128_crypto:encrypt_iv(CState),
                client_nonce => ocb128_crypto:decrypt_iv(CState)
            },
            send_msg(StateData, CryptSetup),
            
            CodecVersion = #{
                message_type => 'CodecVersion',
                alpha => 0,
                beta => 0,
                prefer_alpha => true,
                opus => true
            },
            send_msg(StateData, CodecVersion),
            
            SessionId = maps:get(session_id, UserInfo, 1),
            Username = maps:get(username, UserInfo, ~"Unknown"),
            
            %% Send channel state for root channel BEFORE ServerSync
            send_msg(StateData, #{
                message_type => 'ChannelState',
                channel_id => 0,
                name => ~"Root",
                parent => undefined
            }),
            
            %% Send user state for this user BEFORE ServerSync
            send_msg(StateData, #{
                message_type => 'UserState',
                session => SessionId,
                name => Username,
                channel_id => 0
            }),
            
            ServerSync = #{
                message_type => 'ServerSync',
                session => SessionId,
                max_bandwidth => 128000,
                welcome_text => ~"Welcome"
            },
            send_msg(StateData, ServerSync),
            
            %% Notify user handler that sync is established
            {ok, HState1} = notify_status(Mod, established, SessionId, NewHState),
            
            {transition, established, StateData#state{handler_state = HState1, session_id = SessionId}};
        {error, _Reason} ->
            {stop, normal, StateData}
    end;
handle_protocol_msg(#{message_type := 'Ping'} = Msg, StateData = #state{stats = Stats}) ->
    T = maps:get(timestamp, Msg, 0),
    P = Stats#stats.server_ping,
    send_msg(
        StateData,
        #{
            message_type => 'Ping',
            timestamp => T,
            tcp_packets => Stats#stats.from_client_tcp_packets,
            udp_packets => Stats#stats.from_client_udp_packets,
            good => maybe_undefined(P#ping.good),
            late => maybe_undefined(P#ping.late),
            lost => maybe_undefined(P#ping.lost)
        }
    ),
    notify_stats(StateData#state.stats_events, Stats),
    {ok, StateData};
handle_protocol_msg(#{message_type := 'UDPTunnel', packet := Packet}, StateData = #state{mumble_protocol = Protocol}) ->
    Session = #session{
        id = StateData#state.session_id,
        session_pid = self(),
        mumble_protocol = Protocol
    },
    mumble_udp_proto:handle(Session, Packet),
    %% If it's a voice packet (type != 1), we should fall back to TCP for sending too
    NewStateData = case Packet of
        <<1:3, _/bits>> -> StateData; %% Ping
        _ -> 
            logger:debug("Voice received over TCP tunnel, falling back to TCP for sending"),
            StateData#state{udp_verified = false}
    end,
    {ok, NewStateData};
handle_protocol_msg(Msg, StateData = #state{handler_mod = Mod, handler_state = HState}) ->
    case Mod:handle_msg(Msg, HState) of
        {ok, NewHState} -> {ok, StateData#state{handler_state = NewHState}};
        {stop, _Reason, NewHState} -> {stop, normal, StateData#state{handler_state = NewHState}}
    end.

send_msg(#state{socket = Socket, transport = Transport}, Map) ->
    case mumble_tcp_proto:pack(Map) of
        Bin when is_binary(Bin) ->
            logger:debug("Sending ~p (~p bytes)", [maps:get(message_type, Map), byte_size(Bin)]),
            case Transport:send(Socket, Bin) of
                ok -> ok;
                {error, Reason} ->
                    logger:error("Failed to send ~p: ~p", [maps:get(message_type, Map), Reason]),
                    {error, Reason}
            end
    end.

send_voice(StateData, Msg) ->
    {voice_data, _Type, SenderSession, Target, _Counter, _Voice, _Positional} = Msg,
    case Target of
        31 ->
            %% Server loopback - echo back to sender
            do_send_voice_to_client(StateData, Msg);
        0 ->
            %% Normal talking - broadcast to all users in sender's channel
            ChannelId = StateData#state.channel_id,
            case erlmur_user_manager_available() of
                true ->
                    erlmur_user_manager:broadcast_voice(SenderSession, ChannelId, Msg);
                false ->
                    %% Fallback for testing without erlmur_user_manager
                    logger:debug("erlmur_user_manager not available, echoing voice back to sender"),
                    do_send_voice_to_client(StateData, Msg)
            end;
        _ ->
            %% Whisper targets (1-30) - not implemented yet
            logger:debug("Whisper target ~p not implemented", [Target]),
            ok
    end.

%% Check if erlmur_user_manager is available (running)
erlmur_user_manager_available() ->
    case whereis(erlmur_user_manager) of
        undefined -> false;
        _Pid -> true
    end.

%% Internal function to send voice to the connected client
do_send_voice_to_client(#state{udp_verified = true}, Msg) ->
    Payload = pack_voice(Msg),
    send_udp(self(), Payload);
do_send_voice_to_client(StateData = #state{udp_verified = false}, Msg) ->
    Payload = pack_voice(Msg),
    send_msg(StateData, #{message_type => 'UDPTunnel', packet => Payload}).

pack_voice({voice_data, Type, SenderSession, _Target, Counter, Voice, Positional}) ->
    %% For server->client: use context (0=NORMAL) instead of target
    Context = 0,
    Header = <<Type:3, Context:5>>,
    SenderSessionBin = mumble_varint:encode(SenderSession),
    CounterBin = mumble_varint:encode(Counter),
    Payload = case Positional of
        undefined -> Voice;
        _ -> <<Voice/binary, Positional/binary>>
    end,
    <<Header/binary, SenderSessionBin/binary, CounterBin/binary, Payload/binary>>.

version_enc(#{major := Major, minor := Minor, patch := Patch}) ->
    V1 = (Major bsl 16) bor (Minor bsl 8) bor Patch,
    V2 = (Major bsl 48) bor (Minor bsl 32) bor (Patch bsl 16),
    {V1, V2}.

update_connection_stats(Stats = #stats{server_ping = P}, CryptoStats, Size) ->
    NewP = P#ping{
        good = P#ping.good + CryptoStats#crypto_stats.good,
        late = P#ping.late + CryptoStats#crypto_stats.late,
        lost = P#ping.lost + CryptoStats#crypto_stats.lost
    },
    Stats#stats{
        server_ping = NewP,
        from_client_udp_packets = Stats#stats.from_client_udp_packets + 1,
        from_client_udp_bytes = Stats#stats.from_client_udp_bytes + Size
    }.

notify_status(Mod, Status, SessionId, State) ->
    case Mod:handle_msg(#{message_type => connection_status, status => Status, session_id => SessionId}, State) of
        {ok, NewState} -> {ok, NewState};
        _ -> {ok, State}
    end.

maybe_undefined(0) -> undefined;
maybe_undefined(V) -> V.
