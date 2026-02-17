-module(erlmur_user_manager).
-moduledoc """
Manages active user sessions for the Erlmur server.

This gen_server tracks all connected users and provides APIs for
broadcasting messages and managing session lifecycle.
""".

-behaviour(gen_server).

-include("erlmur.hrl").

%% API
-export([start_link/0]).
-export([register_user/2, unregister_user/1, get_user/1, get_all_users/0]).
-export([broadcast_text/2, broadcast_voice/3]).
-export([register_udp_addr/2, get_session_by_udp/1]).
-export([update_user_channel/2, broadcast_user_state/1]).
-export([get_users_in_channel/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    users :: ets:tid(),
    udp_map :: ets:tid(),
    next_session_id = 1 :: pos_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register_user(Pid :: pid(), Username :: binary()) -> {ok, SessionId :: pos_integer()}.
register_user(Pid, Username) ->
    gen_server:call(?MODULE, {register_user, Pid, Username}).

-spec unregister_user(SessionId :: pos_integer()) -> ok.
unregister_user(SessionId) ->
    gen_server:cast(?MODULE, {unregister_user, SessionId}).

-spec get_user(SessionId :: pos_integer()) -> {ok, #user{}} | {error, not_found}.
get_user(SessionId) ->
    gen_server:call(?MODULE, {get_user, SessionId}).

-spec get_all_users() -> [#user{}].
get_all_users() ->
    gen_server:call(?MODULE, get_all_users).

-spec broadcast_text(FromSessionId :: pos_integer(), Text :: binary()) -> ok.
broadcast_text(FromSessionId, Text) ->
    gen_server:cast(?MODULE, {broadcast_text, FromSessionId, Text}).

-spec broadcast_voice(FromSessionId :: pos_integer(), ChannelId :: non_neg_integer(), VoiceData :: term()) -> ok.
broadcast_voice(FromSessionId, ChannelId, VoiceData) ->
    gen_server:cast(?MODULE, {broadcast_voice, FromSessionId, ChannelId, VoiceData}).

-spec update_user_channel(SessionId :: pos_integer(), ChannelId :: non_neg_integer()) -> ok.
update_user_channel(SessionId, ChannelId) ->
    gen_server:cast(?MODULE, {update_user_channel, SessionId, ChannelId}).

-spec broadcast_user_state(SessionId :: pos_integer()) -> ok.
broadcast_user_state(SessionId) ->
    gen_server:cast(?MODULE, {broadcast_user_state, SessionId}).

-spec register_udp_addr(SessionId :: pos_integer(), Addr :: {inet:ip_address(), inet:port_number()}) -> ok.
register_udp_addr(SessionId, Addr) ->
    gen_server:cast(?MODULE, {register_udp_addr, SessionId, Addr}).

-spec get_session_by_udp(Addr :: {inet:ip_address(), inet:port_number()}) -> {ok, pid()} | {error, not_found}.
get_session_by_udp(Addr) ->
    gen_server:call(?MODULE, {get_session_by_udp, Addr}).

-spec get_users_in_channel(ChannelId :: non_neg_integer()) -> [#user{}].
get_users_in_channel(ChannelId) ->
    gen_server:call(?MODULE, {get_users_in_channel, ChannelId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Users = ets:new(erlmur_users, [set, private, {keypos, #user.session_id}]),
    UdpMap = ets:new(erlmur_udp_map, [set, private]),
    {ok, #state{users = Users, udp_map = UdpMap}}.

handle_call({register_user, Pid, Username}, _From, State = #state{users = Users, next_session_id = SessionId}) ->
    User = #user{session_id = SessionId, pid = Pid, username = Username},
    ets:insert(Users, User),
    monitor(process, Pid),
    logger:info("User ~s registered with session ~p", [Username, SessionId]),
    {reply, {ok, SessionId}, State#state{next_session_id = SessionId + 1}};

handle_call({get_user, SessionId}, _From, State = #state{users = Users}) ->
    case ets:lookup(Users, SessionId) of
        [User] -> {reply, {ok, User}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call(get_all_users, _From, State = #state{users = Users}) ->
    AllUsers = ets:tab2list(Users),
    {reply, AllUsers, State};

handle_call({get_session_by_udp, Addr}, _From, State = #state{udp_map = UdpMap}) ->
    case ets:lookup(UdpMap, Addr) of
        [{_, Pid}] -> {reply, {ok, Pid}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call({get_users_in_channel, ChannelId}, _From, State = #state{users = Users}) ->
    ChannelUsers = ets:foldl(
        fun(#user{channel_id = CId} = User, Acc) when CId == ChannelId ->
                [User | Acc];
           (_, Acc) -> Acc
        end,
        [],
        Users
    ),
    {reply, ChannelUsers, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({unregister_user, SessionId}, State = #state{users = Users, udp_map = UdpMap}) ->
    OldChannel = case ets:lookup(Users, SessionId) of
        [#user{channel_id = ChanId, udp_addr = Addr}] ->
            case Addr of
                undefined -> ok;
                _ -> ets:delete(UdpMap, Addr)
            end,
            ChanId;
        _ -> undefined
    end,
    ets:delete(Users, SessionId),
    logger:info("User with session ~p unregistered", [SessionId]),
    %% Check if old channel should be cleaned up (temporary channel auto-removal)
    case OldChannel of
        undefined -> ok;
        _ -> erlmur_channel_manager:check_and_cleanup_temporary_channel(OldChannel)
    end,
    {noreply, State};

handle_cast({broadcast_text, FromSessionId, Text}, State = #state{users = Users}) ->
    Msg = #{
        message_type => 'TextMessage',
        actor => FromSessionId,
        message => Text
    },
    ets:foldl(
        fun(#user{pid = Pid}, _) ->
            mumble_server_conn:send(Pid, Msg)
        end,
        ok,
        Users
    ),
    {noreply, State};

handle_cast({broadcast_voice, FromSessionId, ChannelId, VoiceData}, State = #state{users = Users}) ->
    ets:foldl(
        fun(#user{session_id = Sid, channel_id = ChanId, pid = Pid}, _) when Sid =/= FromSessionId, ChanId == ChannelId ->
            mumble_server_conn:voice_data(Pid, VoiceData);
           (_, _) -> ok
        end,
        ok,
        Users
    ),
    {noreply, State};

handle_cast({update_user_channel, SessionId, ChannelId}, State = #state{users = Users}) ->
    OldChannel = case ets:lookup(Users, SessionId) of
        [User] ->
            PrevChannel = User#user.channel_id,
            ets:insert(Users, User#user{channel_id = ChannelId}),
            logger:info("User ~s moved from channel ~p to channel ~p", [User#user.username, PrevChannel, ChannelId]),
            PrevChannel;
        [] ->
            logger:warning("Attempted to update channel for unknown session ~p", [SessionId]),
            undefined
    end,
    %% Check if old channel should be cleaned up (temporary channel auto-removal)
    case OldChannel of
        undefined -> ok;
        _ -> erlmur_channel_manager:check_and_cleanup_temporary_channel(OldChannel)
    end,
    {noreply, State};

handle_cast({broadcast_user_state, SessionId}, State = #state{users = Users}) ->
    case ets:lookup(Users, SessionId) of
        [#user{channel_id = ChannelId} = User] ->
            Msg = #{
                message_type => 'UserState',
                session => SessionId,
                channel_id => ChannelId,
                name => User#user.username
            },
            ets:foldl(
                fun(#user{pid = Pid}, _) ->
                    mumble_server_conn:send(Pid, Msg)
                end,
                ok,
                Users
            );
        [] ->
            logger:warning("Attempted to broadcast user state for unknown session ~p", [SessionId])
    end,
    {noreply, State};

handle_cast({register_udp_addr, SessionId, Addr}, State = #state{users = Users, udp_map = UdpMap}) ->
    case ets:lookup(Users, SessionId) of
        [User] ->
            ets:insert(Users, User#user{udp_addr = Addr}),
            ets:insert(UdpMap, {Addr, User#user.pid}),
            logger:debug("Registered UDP addr ~p for session ~p", [Addr, SessionId]);
        [] ->
            logger:warning("Attempted to register UDP addr for unknown session ~p", [SessionId])
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State = #state{users = Users, udp_map = UdpMap}) ->
    %% Find and remove user by Pid
    %% Use foldl to find user without dialyzer issues
    Result = ets:foldl(
        fun(#user{pid = P, session_id = Sid, channel_id = ChanId, udp_addr = Addr}, Acc) ->
            case P of
                Pid -> {found, Sid, ChanId, Addr};
                _ -> Acc
            end
        end,
        not_found,
        Users
    ),
    case Result of
        {found, SessionId, OldChannel, Addr} ->
            logger:info("User with session ~p disconnected", [SessionId]),
            ets:delete(Users, SessionId),
            case Addr of
                undefined -> ok;
                _ -> ets:delete(UdpMap, Addr)
            end,
            %% Check if old channel should be cleaned up (temporary channel auto-removal)
            erlmur_channel_manager:check_and_cleanup_temporary_channel(OldChannel);
        not_found ->
            ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
