-module(erlmur_session).
-moduledoc """
Handles a client's session, including TCP and UDP communication,
and manages the user's state within the Mumble server.

This module is a `gen_statem` that implements the `ranch_protocol`
behavior. It is responsible for the entire lifecycle of a client
connection, from the initial handshake to termination.
""".

-behaviour(gen_statem).
-behaviour(ranch_protocol).

-include("erlmur.hrl").

%% API
-export([start_link/3]).
-export([
    send/2,
    send_udp/2,
    client_version/2,
    user/4, user/1,
    get_state/1,
    update_stats/2,
    move_to_channel/2,
    voice_data/2
]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([authenticating/3, established/3, syncing/3]).

-define(TIMEOUT, 60000).
-define(SYNC_TIMEOUT, 2000).
-record(state, {ref, transport, socket, session, sync_context}).

%%%===================================================================
%%% API
%%%=/home/david/Projects/erlmur/src/erlmur_session.erl==================================================================

start_link(Ref, Transport, Opts) ->
    gen_statem:start_link(?MODULE, {Ref, Transport, Opts}, []).

send(Pid, Msg) ->
    gen_statem:cast(Pid, {send, Msg}).

send_udp(Pid, Msg) ->
    gen_statem:cast(Pid, {send_udp, Msg}).

client_version(Pid, Version) ->
    gen_statem:cast(Pid, {client_version, Version}).

user(Pid, User, Tokens, Type) ->
    gen_statem:cast(Pid, {new_user, User, Tokens, Type}).

user(Pid) ->
    gen_statem:call(Pid, get_user).

get_state(Pid) ->
    gen_statem:call(Pid, get_state).

update_stats(Pid, Stats) ->
    gen_statem:cast(Pid, {update_stats, Stats}).

move_to_channel(Pid, NewChannelId) ->
    gen_statem:cast(Pid, {move_to_channel, NewChannelId}).

voice_data(Pid, Msg) ->
    gen_statem:cast(Pid, Msg).

%%%===================================================================
%%% gen_statem callbacks
%%%=/home/david/Projects/erlmur/src/erlmur_session.erl==================================================================

callback_mode() ->
    [state_functions, state_enter].

init({Ref, Transport, _Opts = []}) ->
    {ok, authenticating,
        #state{
            ref = Ref,
            transport = Transport,
            session = #session{
                id = erlmur_id:new_session_id(),
                session_pid = self(),
                crypto_state = erlmur_crypto:init()
            },
            sync_context = undefined
        },
        ?TIMEOUT}.

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
terminate(Reason, StateName, #state{session = Session} = _StateData) ->
    logger:info("User disconnected state: ~p id: ~p, reason: ~p", [
        StateName, Session#session.id, Reason
    ]),
    pg:leave(pg_erlmur, users, self()),
    pg:leave(pg_erlmur, channels, self()),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%%===================================================================
%%% State Functions
%%%===================================================================

authenticating(enter, _OldState, StateData = #state{ref = Ref, transport = Transport}) ->
    logger:info("New Connection..."),
    pg:join(pg_erlmur, users, self()),
    pg:join(pg_erlmur, channels, self()),
    {ok, Socket} = ranch:handshake(Ref),
    {ok, {Address, _Port}} = Transport:peername(Socket),
    erlmur_session_registry:register_ip(Address, self()),
    ok = Transport:setopts(Socket, [{active, once}]),
    erlmur_tcp_message:send(self(), version),
    {keep_state, StateData#state{socket = Socket}};
authenticating(cast, {client_version, ClientVersion}, StateData = #state{session = Session}) ->
    {keep_state, StateData#state{session = Session#session{client_version = ClientVersion}},
        ?TIMEOUT};
authenticating(cast, {new_user, User, Tokens, Type}, StateData) ->
    handle_new_user(User, Tokens, Type, StateData);
authenticating(info, {ssl, Socket, Data}, StateData) ->
    handle_tcp_data(Socket, Data, StateData, ['Version', 'Authenticate']);
authenticating(cast, {send, Msg}, #state{socket = Socket, transport = Transport}) ->
    Transport:send(Socket, Msg),
    {keep_state_and_data, ?TIMEOUT};
authenticating(Type, Msg, StateData) ->
    logger:warning("State: authenticating~nUnhandled ~p~n~p~n~p", [Type, Msg, StateData]),
    {stop, unhandled}.

established(enter, _OldState, StateData) ->
    User = StateData#state.session#session.user,
    logger:debug("Established user ~p ~p", [User#user.id, User#user.name]),
    {keep_state_and_data, ?TIMEOUT};
established(cast, {new_user_connected, NewUserSession}, #state{session = Session}) ->
    erlmur_tcp_message:send(Session#session.session_pid, {users, [NewUserSession]}),
    {keep_state_and_data, ?TIMEOUT};
established(cast, {get_state_async, ReplyTo, Ref}, #state{session = Session}) ->
    gen_statem:cast(ReplyTo, {state_reply, Ref, Session}),
    {keep_state_and_data, ?TIMEOUT};
established({call, From}, get_state, #state{session = Session}) ->
    gen_statem:reply(From, {ok, Session}),
    {keep_state_and_data, ?TIMEOUT};
established(cast, {send, Msg}, #state{socket = Socket, transport = Transport}) ->
    Transport:send(Socket, Msg),
    {keep_state_and_data, ?TIMEOUT};
established(cast, {send_udp, Msg}, StateData = #state{session = Session}) ->
    {ok, EncryptedMsg, NewCryptoState} = erlmur_crypto:encrypt(Msg, Session#session.crypto_state),
    erlmur_udp_server:send(Session#session.address, Session#session.udp_port, EncryptedMsg),
    {keep_state, StateData#state{session = Session#session{crypto_state = NewCryptoState}},
        ?TIMEOUT};
established(cast, {client_version, ClientVersion}, StateData = #state{session = Session}) ->
    {keep_state, StateData#state{session = Session#session{client_version = ClientVersion}},
        ?TIMEOUT};
established(cast, {update_stats, Stats}, StateData = #state{session = Session}) ->
    {keep_state, StateData#state{session = Session#session{stats = Stats}}, ?TIMEOUT};
established(cast, {move_to_channel, NewChannelId}, StateData = #state{session = Session}) ->
    OldChannelId = Session#session.user#user.channel_id,
    if
        NewChannelId =/= OldChannelId ->
            pg:leave(pg_erlmur, {voice, OldChannelId}, self()),
            pg:join(pg_erlmur, {voice, NewChannelId}, self()),
            NewUser = (Session#session.user)#user{channel_id = NewChannelId},
            erlmur_user_store:update(
                NewUser#user.id, [{channel_id, NewChannelId}], NewUser#user.id
            ),
            {keep_state, StateData#state{session = Session#session{user = NewUser}}, ?TIMEOUT};
        true ->
            {keep_state_and_data, ?TIMEOUT}
    end;
established(
    cast,
    {voice_data, Type, Target, Counter, Voice, Positional},
    #state{session = Session}
) ->
    ChannelId = Session#session.user#user.channel_id,
    case Target of
        % Loopback
        16#1F ->
            erlmur_session:send_udp(
                Session#session.session_pid,
                <<Type:3, 0:5, (erlmur_varint:encode(Session#session.id))/binary,
                    (erlmur_varint:encode(Counter))/binary, Voice/binary>>
            );
        % Normal talk
        16#00 ->
            Pids = pg:get_members(pg_erlmur, {voice, ChannelId}),
            MyPid = Session#session.session_pid,
            OtherPids = lists:delete(MyPid, Pids),
            EncodedSid = erlmur_varint:encode(Session#session.id),
            lists:foreach(
                fun(DestPid) ->
                    erlmur_session:send_udp(
                        DestPid,
                        <<Type:3, 0:5, EncodedSid/binary, (erlmur_varint:encode(Counter))/binary,
                            Voice/binary, Positional/binary>>
                    )
                end,
                OtherPids
            )
    end,
    {keep_state_and_data, ?TIMEOUT};
established({call, From}, get_user, #state{session = #session{user = User}}) ->
    gen_statem:reply(From, {ok, User}),
    {keep_state_and_data, ?TIMEOUT};
established(info, {ssl, Socket, Data}, StateData) ->
    handle_tcp_data(Socket, Data, StateData);
established(info, {udp, IP, PortNo, EncryptedMsg, BroadCast}, StateData) ->
    handle_udp_data(IP, PortNo, EncryptedMsg, BroadCast, StateData);
established(info, {ssl_closed, _Socket}, _StateData) ->
    {stop, normal};
established(info, {ssl_error, _Socket, Reason}, _StateData) ->
    {stop, {ssl_error, Reason}};
established(info, {ssl_passive, _Socket}, _StateData) ->
    logger:warning("ssl_passive"),
    {keep_state_and_data, ?TIMEOUT};
established(timeout, _Msg, _StateData) ->
    logger:warning("Timeout"),
    {stop, normal};
established(Type, Msg, StateData) ->
    logger:warning("State: established~nUnhandled ~p~n~p~n~p", [Type, Msg, StateData]),
    {stop, unhandled}.

syncing(enter, _OldState, _StateData) ->
    logger:debug("Syncing"),
    {keep_state_and_data, ?TIMEOUT};
syncing(
    cast,
    {state_reply, Ref, UserSession},
    #state{sync_context = #{ref := Ref, pending := Pending, replies := Replies}} = StateData
) ->
    NewPending = sets:del_element(UserSession#session.session_pid, Pending),
    NewReplies = [UserSession | Replies],
    case sets:is_empty(NewPending) of
        true ->
            finish_sync(StateData#state.session, NewReplies),
            {next_state, established, StateData#state{sync_context = undefined}};
        false ->
            NewSyncContext = maps:merge(StateData#state.sync_context, #{
                pending => NewPending, replies => NewReplies
            }),
            {keep_state, StateData#state{sync_context = NewSyncContext}}
    end;
syncing(cast, {send, Msg}, #state{socket = Socket, transport = Transport}) ->
    Transport:send(Socket, Msg),
    {keep_state_and_data, ?TIMEOUT};
syncing(timeout, _Msg, #state{session = Session, sync_context = #{replies := Replies}} = StateData) ->
    logger:warning("Timeout while gathering user states. Proceeding with ~p users.", [
        length(Replies)
    ]),
    finish_sync(Session, Replies),
    {next_state, established, StateData#state{sync_context = undefined}};
syncing(Type, Msg, StateData) ->
    logger:warning("State: syncing~nUnhandled ~p~n~p~n~p", [Type, Msg, StateData]),
    {keep_state_and_data}.

%%%===================================================================
%%% Internal functions
%%%=/home/david/Projects/erlmur/src/erlmur_session.erl==================================================================

handle_tcp_data(Socket, Data, StateData) ->
    handle_tcp_data(Socket, Data, StateData, all).

handle_tcp_data(
    Socket, Data, StateData = #state{transport = Transport, session = Session}, Allowed
) when
    byte_size(Data) >= 1
->
    ok = Transport:setopts(Socket, [{active, once}]),
    logger:info("Got data ~p", [Data]),
    DecodedMessages = erlmur_tcp_message:decode(Data),
    lists:foreach(
        fun(Msg) ->
            MsgName = element(1, Msg),
            IsAllowed = Allowed == all orelse lists:member(MsgName, Allowed),
            if
                IsAllowed ->
                    erlmur_tcp_message:handle_message(Session, Msg);
                true ->
                    logger:warning("Message ~p not allowed, allowed messages ~p", [
                        MsgName, Allowed
                    ])
            end
        end,
        DecodedMessages
    ),
    NewStats = erlmur_stats:server_stats({from_client_tcp, byte_size(Data)}, Session#session.stats),
    NewSession = Session#session{stats = NewStats},
    {keep_state, StateData#state{session = NewSession}, ?TIMEOUT}.

handle_udp_data(IP, PortNo, EncryptedMsg, BroadCast, StateData = #state{session = Session}) ->
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
    {keep_state, StateData#state{session = NewSession}, ?TIMEOUT}.

finish_sync(NewSession, UserSessions) ->
    erlmur_tcp_message:send(NewSession#session.session_pid, {users, [NewSession | UserSessions]}),
    erlmur_tcp_message:send(NewSession#session.session_pid, {server_sync, NewSession#session.id}).

handle_new_user(User, Tokens, Type, StateData = #state{session = Session}) ->
    NewSession = Session#session{user = User, temporary_access_tokens = Tokens, type = Type},
    ChannelId = User#user.channel_id,
    pg:join(pg_erlmur, {voice, ChannelId}, self()),
    erlmur_tcp_message:send(
        Session#session.session_pid, {crypto_setup, Session#session.crypto_state}
    ),
    erlmur_tcp_message:send(
        Session#session.session_pid, {codec_version, erlmur_server:codecversion()}
    ),
    Channels = erlmur_channel_store:list(),
    erlmur_tcp_message:send(Session#session.session_pid, {channels, Channels}),

    case
        lists:filter(
            fun(Pid) -> Pid =/= Session#session.session_pid end, pg:get_members(pg_erlmur, users)
        )
    of
        [] ->
            % No other users, finish sync immediately
            finish_sync(NewSession, []),
            logger:debug("User Authenticated ~p ~p", [
                NewSession#session.user#user.id, NewSession#session.user#user.name
            ]),
            {next_state, established, StateData#state{session = NewSession}};
        AllPids ->
            % Other users exist, sync with them
            lists:foreach(
                fun(Pid) ->
                    gen_statem:cast(Pid, {new_user_connected, NewSession})
                end,
                AllPids
            ),

            Ref = make_ref(),
            PidSet = sets:from_list(AllPids),
            SyncContext = #{ref => Ref, pending => PidSet, replies => []},

            lists:foreach(
                fun(Pid) ->
                    gen_statem:cast(Pid, {get_state_async, self(), Ref})
                end,
                AllPids
            ),

            logger:debug("User Authenticated ~p ~p, syncing...", [
                NewSession#session.user#user.id, NewSession#session.user#user.name
            ]),
            {next_state, syncing, StateData#state{session = NewSession, sync_context = SyncContext},
                ?SYNC_TIMEOUT}
    end.
