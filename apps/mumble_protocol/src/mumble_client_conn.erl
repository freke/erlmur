-module(mumble_client_conn).

-moduledoc """
Client connection handler for the Mumble protocol.

This module implements a `gen_statem` that manages a TLS connection to a Mumble
server. It handles the connection lifecycle, authentication, and message exchange.

The connection progresses through these states:
1. `connecting` - Establishing TLS connection
2. `authenticating` - Protocol handshake and authentication  
3. `established` - Active connection ready for voice/data

## Usage

```erlang
{ok, Pid} = mumble_client_conn:start_link("mumble.example.com", 64738, #{}).
ok = mumble_client_conn:send(Pid, #{message_type => 'Ping', timestamp => 12345}).
State = mumble_client_conn:get_state(Pid).
ok = mumble_client_conn:stop(Pid).
```
""".

-behaviour(gen_statem).

-include("mumble_protocol.hrl").
-include("Mumble_gpb.hrl").

%% API
-export([start_link/2, start_link/3, send/2, send_voice/2, get_state/1, stop/1]).
%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3]).
-export([connecting/3, authenticating/3, established/3]).

-record(state, {socket, transport = ssl, session_id, parent, stats = #stats{}, udp_verified = false, udp_timer, opts = #{}}).

-doc """
Start a Mumble client connection with default options.
Input: Host string and port number.
Output: {ok, pid()} on success, {error, term()} on failure.
""".
-spec start_link(string(), inet:port_number()) -> {ok, pid()} | {error, term()}.
start_link(Host, Port) ->
    start_link(Host, Port, #{}).

-doc """
Start a Mumble client connection with custom options.
Input: Host string, port number, and options map.
Output: {ok, pid()} on success, {error, term()} on failure.
""".
-spec start_link(string(), inet:port_number(), map() | list()) -> {ok, pid()} | {error, term()}.
start_link(Host, Port, Opts) when is_map(Opts) ->
    start_link(Host, Port, maps:to_list(Opts));
start_link(Host, Port, Opts) when is_list(Opts) ->
    Parent = self(),
    case gen_statem:start(?MODULE, {Host, Port, Opts, Parent}, []) of
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end.

-doc """
Send a message to the Mumble server through the client connection.
Input: Client PID and message map.
Output: ok (message sent asynchronously).
""".
-spec send(pid(), map()) -> ok.
send(Pid, Msg) ->
    gen_statem:cast(Pid, {send, Msg}).

-doc """
Send voice data to the Mumble server through the client connection.
Input: Client PID and voice data tuple.
Output: ok (voice data sent asynchronously).
""".
-spec send_voice(pid(), {voice_data, non_neg_integer(), non_neg_integer(), non_neg_integer(), binary(), any()}) -> ok.
send_voice(Pid, Msg) ->
    gen_statem:cast(Pid, {send_voice, Msg}).

-doc """
Get the current state of the client connection.
Input: Client PID.
Output: Client state as a map containing connection information.
""".
-spec get_state(pid()) -> map().
get_state(Pid) ->
    gen_statem:call(Pid, get_state).

callback_mode() ->
    [state_functions, state_enter].

init({Host, Port, Opts, Parent}) ->
    {ok, connecting, #state{parent = Parent, opts = Opts}, {next_event, internal, {connect, Host, Port, Opts}}}.

connecting(enter, _, _) ->
    keep_state_and_data;
connecting(internal, {connect, Host, Port, Opts}, State) ->
    CertFile = proplists:get_value(cert_file, Opts),
    KeyFile = proplists:get_value(key_file, Opts),
    SslOptsList = case {CertFile, KeyFile} of
        {undefined, undefined} ->
            [{active, once}, binary, {verify, verify_none}, {versions, ['tlsv1.2', 'tlsv1.3']}];
        _ ->
            [{active, once}, binary, {verify, verify_none}, {versions, ['tlsv1.2', 'tlsv1.3']},
             {certfile, CertFile}, {keyfile, KeyFile}]
    end,
    case ssl:connect(Host, Port, SslOptsList) of
        {ok, Socket} ->
            {next_state, authenticating, State#state{socket = Socket}};
        {error, Reason} ->
            logger:error("Client failed to connect to ~p:~p reason: ~p", [Host, Port, Reason]),
            %% Stop gracefully with connection error
            {stop, {connection_failed, Reason}}
    end;
connecting(Type, Msg, State) ->
    handle_common(connecting, Type, Msg, State).

authenticating(enter, _, State) ->
    %% Send Version
    V = #version{major = 1,
                 minor = 2,
                 patch = 4},
    {V1, V2} = mumble_version:encode(V),
    VerMsg =
        #{message_type => 'Version',
          version_v1 => V1,
          version_v2 => V2,
          release => <<"erlmur-client">>},
    ssl:send(State#state.socket, mumble_tcp_proto:pack(VerMsg)),
    %% Send Authenticate with credentials from opts
    Username = proplists:get_value(username, State#state.opts, <<>>),
    Password = proplists:get_value(password, State#state.opts, <<>>),
    Token = proplists:get_value(token, State#state.opts, <<>>),
    AuthMsg = #{
        message_type => 'Authenticate',
        username => Username,
        password => Password,
        token => Token
    },
    ssl:send(State#state.socket, mumble_tcp_proto:pack(AuthMsg)),
    ssl:setopts(State#state.socket, [{active, once}]),
    keep_state_and_data;
authenticating(info, {ssl, _Socket, Data}, State = #state{stats = Stats}) ->
    NewStats = Stats#stats{
        from_client_tcp_packets = Stats#stats.from_client_tcp_packets + 1,
        from_client_tcp_bytes = Stats#stats.from_client_tcp_bytes + byte_size(Data)
    },
    NewState = State#state{stats = NewStats},
    Msgs = mumble_tcp_proto:decode(Data),
    lists:foreach(fun(M) -> 
        logger:debug("Client received during auth: ~p", [maps:get(message_type, M)]),
        State#state.parent ! {mumble_msg, M}
    end, Msgs),
    case [M || M = #{message_type := 'ServerSync'} <- Msgs] of
        [#{session := SessionId} | _] ->
            {next_state, established, NewState#state{session_id = SessionId}};
        _ ->
            ssl:setopts(State#state.socket, [{active, once}]),
            {keep_state, NewState}
    end;
authenticating(Type, Msg, State) ->
    handle_common(authenticating, Type, Msg, State).

established(cast, {send, Msg}, State) ->
    ssl:send(State#state.socket, mumble_tcp_proto:pack(Msg)),
    keep_state_and_data;
established(info, {ssl, _Socket, Data}, State = #state{stats = Stats}) ->

    NewStats = Stats#stats{
        from_client_tcp_packets = Stats#stats.from_client_tcp_packets + 1,
        from_client_tcp_bytes = Stats#stats.from_client_tcp_bytes + byte_size(Data)
    },
    Msgs = mumble_tcp_proto:decode(Data),
    lists:foreach(fun(M) -> 
        logger:debug("Client received: ~p", [maps:get(message_type, M)]),
        %% Forward to parent
        State#state.parent ! {mumble_msg, M},
        case M of
            #{message_type := 'Ping', timestamp := T} ->
                %% Auto reply to server ping with our stats
                Reply = #{
                    message_type => 'Ping',
                    timestamp => T,
                    tcp_packets => NewStats#stats.from_client_tcp_packets,
                    udp_packets => NewStats#stats.from_client_udp_packets
                },
                ssl:send(State#state.socket, mumble_tcp_proto:pack(Reply));
            _ -> ok
        end
    end, Msgs),
    ssl:setopts(State#state.socket, [{active, once}]),
    {keep_state, State#state{stats = NewStats}};
established(info, {timeout, _Ref, udp_ping}, State) ->
    %% Send UDP Ping if we want to verify UDP
    send_udp_ping(State),
    erlang:start_timer(5000, self(), udp_ping),
    keep_state_and_data;
established(info, {udp, _Socket, _Host, _Port, _Packet}, State) ->
    %% Received UDP packet, so UDP is verified
    logger:notice("Client UDP verified"),
    {keep_state, State#state{udp_verified = true}};
established(cast, {send_voice, Msg}, State) ->
    do_send_voice(State, Msg),
    keep_state_and_data;
established(enter, OldState, State) ->
    %% Start UDP Ping timer
    erlang:start_timer(1000, self(), udp_ping),
    handle_common(established, enter, OldState, State);
established(Type, Msg, State) ->
    handle_common(established, Type, Msg, State).

handle_common(_StateName, {call, From}, get_state, State) ->
    %% Return simplified state as a map instead of the full record
    SimpleState = #{
        socket => State#state.socket,
        transport => State#state.transport,
        session_id => State#state.session_id,
        connected => State#state.socket =/= undefined,
        opts => State#state.opts
    },
    {keep_state, State, [{reply, From, SimpleState}]};
handle_common(_StateName, info, {ssl_closed, _}, _State) ->
    {stop, normal};
handle_common(_StateName, enter, _OldState, _State = #state{socket = undefined}) ->
    %% No socket yet (connection failed or not yet established)
    keep_state_and_data;
handle_common(_StateName, enter, _OldState, State) ->
    ssl:setopts(State#state.socket, [{active, once}]),
    keep_state_and_data;
handle_common(StateName, Type, Msg, _State) ->
    logger:warning("Client ~p unhandled ~p ~p", [StateName, Type, Msg]),
    keep_state_and_data.

do_send_voice(State = #state{udp_verified = true}, Msg) ->
    Payload = pack_voice(Msg),
    send_udp(State, Payload);
do_send_voice(State = #state{udp_verified = false}, Msg) ->
    Payload = pack_voice(Msg),
    ssl:send(State#state.socket, mumble_tcp_proto:pack(#{message_type => 'UDPTunnel', packet => Payload})).

send_udp_ping(_State) ->
    %% NOTE: Actual UDP send should happen here. 
    %% Mumble UDP Ping is <<1:3, 0:5, (64-bit timestamp)>> or similar.
    logger:debug("Client would send UDP Ping here").

send_udp(_State, _Payload) ->
    %% NOTE: Actual UDP send should happen here.
    logger:debug("Client would send UDP packet here").

pack_voice({voice_data, Type, Target, Counter, Voice, Positional}) ->
    Header = <<Type:3, Target:5>>,
    CounterBin = mumble_varint:encode(Counter),
    Payload = case Positional of
        undefined -> Voice;
        _ -> <<Voice/binary, Positional/binary>>
    end,
    <<Header/binary, CounterBin/binary, Payload/binary>>.

-doc """
Stop the client connection and clean up resources.
Input: Client PID.
Output: ok (stops the gen_statem process).
""".
-spec stop(pid()) -> ok | {error, term()}.
stop(Pid) ->
    try
        gen_statem:stop(Pid),
        ok
    catch
        exit:noproc ->
            {error, invalid_client_ref};
        exit:Reason ->
            {error, Reason}
    end.

terminate(_Reason, _State, #state{socket = Socket}) ->
    catch ssl:close(Socket),
    ok.
