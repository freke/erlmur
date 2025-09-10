-module(erlmur_session).
-behaviour(gen_statem).
-behaviour(ranch_protocol).

-include("erlmur.hrl").

-export([start_link/3]).
-export([send/2, send_udp/2, client_version/2, user/4, get_state/1, update_stats/2]).
-export([callback_mode/0]).
-export([init/1]).
-export([connected/3]).
-export([terminate/3]).
-export([code_change/4]).

-define(TIMEOUT, 60000).
-record(state, {ref, transport, socket, session}).

start_link(Ref, Transport, Opts) ->
    gen_statem:start_link(?MODULE, {Ref, Transport, Opts}, []).

send_udp(Pid, Msg) ->
    gen_statem:cast(Pid, {send_udp, Msg}).

send(Pid, Msg) ->
    gen_statem:cast(Pid, {send, Msg}).

client_version(Pid, Version) ->
    gen_statem:cast(Pid, {client_version, Version}).

user(Pid, User, Tokens, IsBot) ->
    gen_statem:cast(Pid, {new_user, User, Tokens, IsBot}).

get_state(Pid) ->
    gen_statem:call(Pid, get_state).

update_stats(Pid, Stats) ->
    gen_statem:cast(Pid, {update_stats, Stats}).

callback_mode() ->
    [state_functions, state_enter].

init({Ref, Transport, _Opts = []}) ->
    {ok, connected,
        #state{
            ref = Ref,
            transport = Transport,
            session = #session{id = 99, session_pid = self(), crypto_state = erlmur_crypto:init()}
        },
        ?TIMEOUT}.

connected(
    enter, connected, StateData = #state{ref = Ref, transport = Transport}
) ->
    logger:info("New Connection..."),
    pg:join(pg_erlmur, users, self()),
    pg:join(pg_erlmur, channels, self()),
    {ok, Socket} = ranch:handshake(Ref),
    {ok, {Address, _Port}} = Transport:peername(Socket),
    erlmur_session_registry:register_ip(Address, self()),
    ok = Transport:setopts(Socket, [{active, once}]),
    erlmur_tcp_message:send(self(), version),
    {keep_state, StateData#state{socket = Socket}};
connected(
    info,
    {ssl, Socket, Data},
    StateData = #state{socket = Socket, transport = Transport, session = Session}
) when byte_size(Data) >= 1 ->
    ok = Transport:setopts(Socket, [{active, once}]),
    logger:info("Got data ~p", [Data]),
    erlmur_tcp_message:handle(Session, Data),
    NewStats = erlmur_stats:server_stats({from_client_tcp, byte_size(Data)}, Session#session.stats),
    NewSession = Session#session{stats = NewStats},
    {keep_state, StateData#state{session = NewSession}, ?TIMEOUT};
connected(info, {udp, IP, PortNo, EncryptedMsg, BroadCast}, StateData = #state{session = Session}) ->
    NewSession =
        case erlmur_crypto:decrypt(EncryptedMsg, Session#session.crypto_state) of
            {ok, Msg, UpdateStats, NewCryptoState} ->
                CryptoStats = erlmur_stats:server_stats(UpdateStats, Session#session.stats),
                NewStats =
                    erlmur_stats:server_stats(
                        {from_client_udp, byte_size(EncryptedMsg)}, CryptoStats
                    ),
                UpdatedSession = Session#session{
                    crypto_state = NewCryptoState,
                    stats = NewStats,
                    address = IP,
                    udp_port = PortNo,
                    use_udp_tunnel = false
                },
                erlmur_udp_message:handle(UpdatedSession, Msg),
                if
                    BroadCast ->
                        erlmur_session_registry:register_ip_port(IP, PortNo, self());
                    true ->
                        ok
                end,
                UpdatedSession;
            {error, Reason, UpdateStats, NewCryptoState} ->
                logger:error("UDP Decrypt failed: ~p", [Reason]),
                NewStats = erlmur_stats:server_stats(UpdateStats, Session#session.stats),
                Session#session{
                    crypto_state = NewCryptoState, stats = NewStats, address = IP, udp_port = PortNo
                }
        end,
    {keep_state, StateData#state{session = NewSession}};
connected(info, {ssl_closed, _Socket}, _StateData) ->
    {stop, normal};
connected(info, {ssl_error, _Socket, Reason}, _StateData) ->
    {stop, Reason};
connected(info, {ssl_passive, _Socket}, _StateData) ->
    logger:warning("ssl_passive"),
    {keep_state_and_data, ?TIMEOUT};
connected({call, From}, get_state, #state{session = Session}) ->
    gen_statem:reply(From, {ok, Session}),
    keep_state_and_data;
connected(cast, {send, Msg}, _StateData = #state{socket = Socket, transport = Transport}) ->
    Transport:send(Socket, Msg),
    keep_state_and_data;
connected(cast, {client_version, ClientVersion}, StateData = #state{session = Session}) ->
    {keep_state, StateData#state{session = Session#session{client_version = ClientVersion}}};
connected(cast, {update_stats, Stats}, StateData = #state{session = Session}) ->
    {keep_state, StateData#state{session = Session#session{stats = Stats}}};
connected(cast, {new_user, User, Tokens, IsBot}, StateData = #state{session = Session}) ->
    NewSession = Session#session{user = User, temporary_access_tokens = Tokens, is_bot = IsBot},
    erlmur_tcp_message:send(
        Session#session.session_pid, {crypto_setup, Session#session.crypto_state}
    ),
    Channels = erlmur_channel_store:list(),
    erlmur_tcp_message:send(Session#session.session_pid, {channels, Channels}),
    AllPids = lists:filter(
        fun(Pid) -> Pid =/= Session#session.session_pid end, pg:get_members(pg_erlmur, users)
    ),
    UserSessions =
        lists:flatmap(
            fun(Pid) ->
                case erlmur_session:get_state(Pid) of
                    {ok, #session{user = #user{}} = S} ->
                        [S];
                    _ ->
                        []
                end
            end,
            AllPids
        ),
    erlmur_tcp_message:send(Session#session.session_pid, {users, [NewSession | UserSessions]}),
    erlmur_tcp_message:send(Session#session.session_pid, {server_sync, Session#session.id}),
    {keep_state, StateData#state{session = NewSession}};
connected(cast, {send_udp, Msg}, StateData = #state{session = Session}) ->
    {ok, EncryptedMsg, NewCryptoState} = erlmur_crypto:encrypt(Msg, Session#session.crypto_state),
    erlmur_udp_server:send(Session#session.address, Session#session.udp_port, EncryptedMsg),
    {keep_state, StateData#state{session = Session#session{crypto_state = NewCryptoState}}};
connected(timeout, _Msg, _StateData) ->
    logger:warning("Timeout"),
    {stop, normal};
connected(_EventType, _Msg, _StateData) ->
    logger:error("Unknown session event ~p, ~p", [_EventType, _Msg]),
    {stop, normal}.

terminate(
    Reason,
    StateName,
    StateData = #state{
        socket = Socket, transport = Transport
    }
) when
    Socket =/= undefined, Transport =/= undefined
->
    catch Transport:close(Socket),
    terminate(Reason, StateName, StateData#state{socket = undefined, transport = undefined});
terminate(_Reason, _StateName, _StateData) ->
    pg:leave(pg_erlmur, users, self()),
    pg:leave(pg_erlmur, channels, self()),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
