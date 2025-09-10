-module(erlmur_server).

-moduledoc """
Manages the overall state and configuration of the Erlmur server.


This module handles server-wide settings, authentication, and delegates
requests to other specialized modules for processing.
""".

-behaviour(gen_server).

-include("erlmur.hrl").

%% API
-export([
    start_link/0,
    version/0,
    authenticate/2,
    channelstates/0,
    channelstate/1,
    userstates/0,
    userstate/1,
    userremove/4,
    codecversion/0, codecversion/2,
    config/0,
    usercount/0,
    permissionquery/1,
    voice_data/6,
    list_banlist/0
]).
%% gen_server callbacks
-export([
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

-record(state, {version, codec_version = #codec_version{}, server_config = #server_config{}}).

-opaque version() :: #version{}.

-export_type([version/0]).

-opaque codec_version() :: #codec_version{}.

-export_type([codec_version/0]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

version() ->
    gen_server:call(?SERVER, version).

authenticate(User, Password) ->
    gen_server:call(?SERVER, {authenticate, User, Password}).

channelstates() ->
    gen_server:call(?SERVER, channelstates).

channelstate(ChannelState) ->
    gen_server:cast(?SERVER, {channelstate, ChannelState}).

userstates() ->
    gen_server:call(?SERVER, userstates).

userstate(UserState) ->
    gen_server:cast(?SERVER, {userstate, UserState}).

userremove(Session, Actor, Reason, Ban) ->
    gen_server:cast(?SERVER, {userremove, Session, Actor, Reason, Ban}).

codecversion() ->
    gen_server:call(?SERVER, codecversion).

codecversion(Codec, Opus) ->
    gen_server:cast(?SERVER, {codecversion, Codec, Opus}).

config() ->
    gen_server:call(?SERVER, serverconfig).

permissionquery(ChannelId) ->
    gen_server:call(?SERVER, {permissionquery, ChannelId}).

usercount() ->
    gen_server:call(?SERVER, usercount).

voice_data(Type, Target, ClientPid, Counter, Voice, Positional) ->
    gen_server:cast(
        ?SERVER,
        {voice_data, Type, Target, ClientPid, Counter, Voice, Positional}
    ).

list_banlist() ->
    [].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Nodes = [node()],
    ChannelTables = erlmur_channel_store:init(Nodes),
    UserTables = erlmur_user_store:init(Nodes),
    {OsFamily, OsName} = os:type(),
    OsNameString = io_lib:format("~w-~w", [OsFamily, OsName]),
    OsVersion =
        case os:version() of
            {OsMajor, OsMinor, OsRelease} ->
                io_lib:format("~w.~w.~w", [OsMajor, OsMinor, OsRelease]);
            V ->
                V
        end,
    {ok, VsnStr} = application:get_key(erlmur, vsn),
    Version =
        #version{
            major = ?MUMBLE_PROTOCOL_VERSION_MAJOR,
            minor = ?MUMBLE_PROTOCOL_VERSION_MINOR,
            patch = ?MUMBLE_PROTOCOL_VERSION_PATCH,
            release = iolist_to_binary(io_lib:format("erlmur ~s", [VsnStr])),
            os = iolist_to_binary(OsNameString),
            os_version = iolist_to_binary(OsVersion)
        },
    ok = mnesia:wait_for_tables(UserTables ++ ChannelTables, 5000),
    erlmur_channel_store:create_default(),
    {ok, #state{version = Version}}.

handle_call(version, _From, State) ->
    {reply, State#state.version, State};
handle_call({authenticate, UserName, Pass}, {Pid, _}, State) ->
    logger:info("erlmur_server::handle_call~nAuthenticate: ~p~nPass: ~p", [UserName, Pass]),
    User = erlmur_users:add(UserName),
    erlmur_session_monitor:monitor(Pid),
    {reply, {ok, User}, State};
handle_call(all_channels, _From, State) ->
    Channels = erlmur_channels:list(),
    {reply, Channels, State};
handle_call(all_users, _from, State) ->
    Users = erlmur_users:list(),
    {reply, Users, State};
handle_call(codecversion, _From, State) ->
    {reply, State#state.codec_version, State};
handle_call(serverconfig, _From, State) ->
    {reply, State#state.server_config, State};
handle_call({permissionquery, ChannelId}, {Pid, _}, State) ->
    NewPerm =
        case ChannelId of
            undefined ->
                {ChannelId, -1};
            _ ->
                Channel = erlmur_channels:fetch({id, ChannelId}),
                {ok, SessionRecord} = erlmur_session_registry:lookup({session_pid, Pid}),
                User = erlmur_users:fetch({user_id, SessionRecord#session_record.user_id}),
                Permissions = permissions(Channel, User),
                {ChannelId, Permissions}
        end,
    {reply, NewPerm, State};
handle_call(usercount, _From, State) ->
    NumUsers = proplists:get_value(workers, supervisor:count_children(erlmur_session_sup)),
    {reply, NumUsers, State};
handle_call({channel_filter, Filter}, _From, State) ->
    FilterdChannels = erlmur_channels:filter(Filter),
    {reply, FilterdChannels, State};
handle_call(Request, _From, State) ->
    error_logger:info_report([{erlmur_server, handle_call}, {unhandled_request, Request}]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast({codecversion, _C, _Opus}, State) ->
    logger:error("Todo: Handle codec version ~p ~p", [_C, _Opus]),
    {noreply, State};
handle_cast({voice_data, Type, 16#1F, Pid, Counter, Voice, _Positional}, State) ->
    {ok, SessionRecord} = erlmur_session_registry:lookup({session_pid, Pid}),
    C = erlmur_varint:encode(Counter),
    EncodedSid = erlmur_varint:encode(SessionRecord#session_record.session_id),
    erlmur_session:send_udp(Pid, <<Type:3, 0:5, EncodedSid/binary, C/binary, Voice/binary>>),
    {noreply, State};
handle_cast({voice_data, Type, 16#00, Pid, Counter, Voice, Positional}, State) ->
    {ok, SessionRecord} = erlmur_session_registry:lookup({session_pid, Pid}),
    User = erlmur_users:fetch({user_id, SessionRecord#session_record.user_id}),
    Users =
        lists:filter(
            fun(U) -> U#user.id =/= SessionRecord#session_record.user_id end,
            erlmur_users:fetch({id, User#user.id})
        ),
    C = erlmur_varint:encode(Counter),
    EncodedSid = erlmur_varint:encode(SessionRecord#session_record.session_id),
    error_logger:info_report([{erlmur_server, voice_data}, {users, Users}]),
    lists:foreach(
        fun(U) ->
            {ok, Rec} = erlmur_session_registry:lookup({user_id, U#user.id}),
            erlmur_session:send_udp(
                Rec#session_record.session_pid,
                <<Type:3, 0:5, EncodedSid/binary, C/binary, Voice/binary, Positional/binary>>
            )
        end,
        Users
    ),
    {noreply, State};
handle_cast({channelstate, #{id := ChannelId} = Update}, State) ->
    case ChannelId of
        undefined ->
            erlmur_channels:add(Update);
        ChannelId ->
            erlmur_channels:update(ChannelId, Update)
    end,
    {noreply, State};
handle_cast({channelremove, PropList, Actor}, State) ->
    erlmur_channels:remove(
        proplists:get_value(id, PropList), Actor
    ),
    {noreply, State};
handle_cast({userremove, SessionId, Actor, Reason, Ban}, State) ->
    logger:info("User ~p removed user ~p", [Actor, SessionId]),
    {ok, SessionRecord} = erlmur_session_registry:lookup({session_id, SessionId}),
    erlmur_users:remove(SessionRecord#session_record.user_id, SessionId, Actor, Reason, Ban),
    erlmur_session:stop(SessionRecord#session_record.session_pid),
    {noreply, State};
handle_cast(Msg, State) ->
    error_logger:info_report([{erlmur_server, handle_cast}, {"Unhandle msg", Msg}]),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:info_report([{erlmur_server, handle_info}, {"Unhandle info", Info}]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% Stream of integers %%%
permissions(_Channel, _User) ->
    %?PERM_NONE.
    ?PERM_ALL.
