-module(erlmur_tcp_message).

-moduledoc """
Handles the packing and unpacking of Mumble protocol messages.

This module serializes Erlang records into binary format for network transmission
and deserializes incoming binary data into Erlang records.
""".

-export([send/2, decode/1, handle_message/2]).

-include("Mumble_gpb.hrl").
-include("erlmur.hrl").

%% 0
-define(MSG_VERSION, 16#00).
-define(MSG_UDPTUNNEL, 16#01).
-define(MSG_AUTHENTICATE, 16#02).
-define(MSG_PING, 16#03).
% -define(MSG_REJECT, 16#04).
-define(MSG_SERVERSYNC, 16#05).
-define(MSG_CHANNELREMOVE, 16#06).
-define(MSG_CHANNELSTATE, 16#07).
-define(MSG_USERREMOVE, 16#08).
-define(MSG_USERSTATE, 16#09).
%% 10
-define(MSG_BANLIST, 16#0A).
-define(MSG_TEXTMESSAGE, 16#0B).
% -define(MSG_PERMISSONDENIED, 16#0C).
% -define(MSG_ACL, 16#0D).
% -define(MSG_QUERYUSERS, 16#0E).
-define(MSG_CRYPTSETUP, 16#0F).
% -define(MSG_CONTEXTACTIONADD, 16#10).
% -define(MSG_CONTEXTACTION, 16#11).
-define(MSG_USERLIST, 16#12).
% -define(MSG_VOICETARGET, 16#13).
%% 20
-define(MSG_PERMISSIONQUERY, 16#14).
-define(MSG_CODECVERSION, 16#15).
-define(MSG_USERSTATS, 16#16).
% -define(MSG_REQUESTBLOB, 16#17).
-define(MSG_SERVERCONFIG, 16#18).
% -define(MSG_SUGGESTCONFIG, 16#19).
-define(MESSAGE_TABLE, [
    {?MSG_VERSION, #'Version'{}},
    {?MSG_UDPTUNNEL, #'UDPTunnel'{}},
    {?MSG_AUTHENTICATE, #'Authenticate'{}},
    {?MSG_PING, #'Ping'{}},
    {?MSG_SERVERSYNC, #'ServerSync'{}},
    {?MSG_USERSTATE, #'UserState'{}},
    {?MSG_USERREMOVE, #'UserRemove'{}},
    {?MSG_USERLIST, #'UserList'{}},
    {?MSG_USERSTATS, #'UserStats'{}},
    {?MSG_BANLIST, #'BanList'{}},
    {?MSG_TEXTMESSAGE, #'TextMessage'{}},
    {?MSG_CRYPTSETUP, #'CryptSetup'{}},
    {?MSG_CHANNELSTATE, #'ChannelState'{}},
    {?MSG_CHANNELREMOVE, #'ChannelRemove'{}},
    {?MSG_CODECVERSION, #'CodecVersion'{}},
    {?MSG_SERVERCONFIG, #'ServerConfig'{}},
    {?MSG_PERMISSIONQUERY, #'PermissionQuery'{}}
]).

-doc """
Sends a message to a client session.

This function packs a given message into the Mumble TCP protocol format and sends it
to the specified session process. The message to be sent is determined by the second
argument, which can be one of several atoms or tuples corresponding to different
Mumble protocol messages.
""".
-spec send(
    SessionPid :: pid(),
    Msg ::
        version
        | {crypto_setup, any()}
        | {channels, list()}
        | {users, list()}
        | {server_sync, integer()}
        | {codec_version, any()}
) -> ok.
send(SessionPid, version) ->
    ServerVersion = erlmur_server:version(),
    {V1, V2} = erlmur_protocol_version:encode(ServerVersion),
    Version = #'Version'{
        version_v1 = V1,
        version_v2 = V2,
        os = ServerVersion#version.os,
        release = ServerVersion#version.release,
        os_version = ServerVersion#version.os_version
    },
    Msg = pack(Version),
    logger:debug("Send version ~p", [Version]),
    erlmur_session:send(SessionPid, Msg);
send(SessionPid, {crypto_setup, CryptoState}) ->
    CryptSetup = #'CryptSetup'{
        key = erlmur_crypto:key(CryptoState),
        server_nonce = erlmur_crypto:encrypt_iv(CryptoState),
        client_nonce = erlmur_crypto:decrypt_iv(CryptoState)
    },
    Msg = pack(CryptSetup),
    erlmur_session:send(SessionPid, Msg);
send(SessionPid, {channels, Channels}) ->
    logger:debug("Send Channels"),
    lists:foreach(
        fun(C) ->
            ChannelState = #'ChannelState'{
                channel_id = C#channel.id,
                parent = C#channel.parent_id,
                name = C#channel.name,
                description = C#channel.description,
                temporary = C#channel.temporary,
                position = C#channel.position,
                description_hash = C#channel.description_hash,
                max_users = C#channel.max_users,
                is_enter_restricted = C#channel.is_enter_restricted,
                can_enter = C#channel.can_enter
            },
            logger:debug("Send ChannelState: ~p", [ChannelState]),
            Msg = pack(ChannelState),
            erlmur_session:send(SessionPid, Msg)
        end,
        Channels
    ),
    UpdateLinks = lists:filter(fun(C) -> not sets:is_empty(C#channel.links) end, Channels),
    lists:foreach(
        fun(C) ->
            ChannelState = #'ChannelState'{
                channel_id = C#channel.id, links = sets:to_list(C#channel.links)
            },
            logger:debug("Send ChannelState links: ~p", [ChannelState]),
            Msg = pack(ChannelState),
            erlmur_session:send(SessionPid, Msg)
        end,
        UpdateLinks
    );
send(SessionPid, {users, UserSessions}) ->
    logger:debug("Send Users"),
    lists:foreach(
        fun(US) ->
            UserState = #'UserState'{
                session = US#session.id,
                name = US#session.user#user.name,
                user_id = US#session.user#user.id,
                channel_id = US#session.user#user.channel_id,
                mute = US#session.mute,
                deaf = US#session.deaf,
                texture = US#session.user#user.texture,
                comment = US#session.user#user.comment,
                hash = US#session.user#user.hash,
                comment_hash = US#session.user#user.comment_hash,
                texture_hash = US#session.texture_hash,
                priority_speaker = US#session.priority_speaker,
                recording = US#session.recording
            },
            logger:debug("Send UserState ~p", [UserState]),
            Msg = pack(UserState),
            erlmur_session:send(SessionPid, Msg)
        end,
        UserSessions
    );
send(SessionPid, {server_sync, SessionId}) ->
    logger:debug("Send Server Sync"),
    ServerConfig = erlmur_server:config(),
    ServerSync = #'ServerSync'{
        max_bandwidth = ServerConfig#server_config.max_bandwidth,
        welcome_text = ServerConfig#server_config.welcome_text,
        session = SessionId
    },
    erlmur_session:send(SessionPid, pack(ServerSync));
send(SessionPid, {codec_version, CodecVersion}) ->
    CodecVersionMsg = #'CodecVersion'{
        alpha = CodecVersion#codec_version.alpha,
        beta = CodecVersion#codec_version.beta,
        prefer_alpha = CodecVersion#codec_version.prefer_alpha,
        opus = CodecVersion#codec_version.opus
    },
    Msg = pack(CodecVersionMsg),
    erlmur_session:send(SessionPid, Msg).

-doc """
Decodes a binary payload from a TCP message into a list of Mumble message records.

The function processes the binary data and returns a list of decoded records.
If the payload is incomplete or contains an unknown message type, it logs an error
and attempts to decode the rest of the payload.
""".
-spec decode(binary()) -> [any()].
decode(Data) ->
    unpack(Data, []).

-doc """
Handles a decoded Mumble message for a given session.

This function takes a session state and a message record, and performs the
appropriate action based on the message type.
""".
-spec handle_message(any(), any()) -> ok.
handle_message(
    Session,
    #'Version'{
        version_v1 = V1, version_v2 = undefined, os = Os, os_version = OsVersion, release = Release
    }
) ->
    Version = erlmur_protocol_version:decode(V1),
    ClientVersion = Version#version{release = Release, os = Os, os_version = OsVersion},
    logger:info("Version v1 ~p", [ClientVersion]),
    erlmur_session:client_version(Session#session.session_pid, ClientVersion);
handle_message(
    Session,
    #'Version'{version_v2 = V2, os = Os, os_version = OsVersion, release = Release}
) ->
    Version = erlmur_protocol_version:decode(V2),
    ClientVersion = Version#version{release = Release, os = Os, os_version = OsVersion},
    logger:info("Version v2 ~p", [ClientVersion]),
    erlmur_session:client_version(Session#session.session_pid, ClientVersion);
handle_message(Session, #'Authenticate'{
    username = Username,
    password = Password,
    opus = Opus,
    tokens = Tokens,
    client_type = Type,
    celt_versions = CeltVersions
}) ->
    logger:info("Authenticate user ~p", [Username]),
    T =
        case Type of
            0 -> regular;
            1 -> bot
        end,
    case erlmur_authenticate:check(Username, Password) of
        {ok, User} ->
            erlmur_server:codecversion(CeltVersions, Opus),
            erlmur_session:user(Session#session.session_pid, User, Tokens, T)
    end;
handle_message(
    Session,
    #'UserState'{session = SessionId, channel_id = ChannelId} = UserState
) ->
    if
        SessionId == Session#session.id andalso ChannelId =/= undefined ->
            erlmur_session:move_to_channel(Session#session.session_pid, ChannelId);
        true ->
            logger:info("Got UserState ~p", [UserState]),
            logger:warning("TODO: Handle UserState update")
    end;
handle_message(_Session, UserStats = #'UserStats'{}) ->
    logger:info("Got UserStats ~p", [UserStats]),
    logger:warning("TODO: Handle UserStats");
handle_message(Session, #'PermissionQuery'{channel_id = ChannelId}) ->
    logger:debug("PermissionQuery ~p", [ChannelId]),
    {PermChannelId, Permissions} = erlmur_acl:query_permissions(
        ChannelId, Session#session.user#user.id
    ),
    Response = #'PermissionQuery'{channel_id = PermChannelId, permissions = Permissions},
    erlmur_session:send(Session#session.session_pid, pack(Response));
handle_message(
    Session,
    #'Ping'{
        udp_packets = UdpPacket,
        tcp_packets = TcpPackets,
        udp_ping_avg = UdpPingAvg,
        udp_ping_var = UdpPingVar,
        tcp_ping_avg = TcpPingAvg,
        tcp_ping_var = TcpPingVar,
        good = PingGood,
        late = PingLate,
        lost = PingLost,
        resync = PingResync,
        timestamp = Timestamp
    } = Ping
) ->
    logger:info("Got Ping ~p", [Ping]),
    S1 = erlmur_stats:packets({UdpPacket, TcpPackets}, Session#session.stats),
    S2 = erlmur_stats:times({UdpPingAvg, UdpPingVar, TcpPingAvg, TcpPingVar}, S1),
    NewStats = erlmur_stats:client_stats({PingGood, PingLate, PingLost, PingResync}, S2),

    erlmur_session:update_stats(Session#session.session_pid, NewStats),

    Pong =
        #'Ping'{
            timestamp = Timestamp,
            good = NewStats#stats.server_ping#ping.good,
            late = NewStats#stats.server_ping#ping.late,
            lost = NewStats#stats.server_ping#ping.lost,
            resync = NewStats#stats.server_ping#ping.resync
        },

    Msg = pack(Pong),
    erlmur_session:send(Session#session.session_pid, Msg);
handle_message(_Session, ChannelState = #'ChannelState'{channel_id = undefined}) ->
    logger:info("Request to create channel: ~p", [ChannelState]),
    % TODO: Check permissions
    ChannelMap = channel_state_to_map(ChannelState),
    FilteredMap = maps:without([links, links_add, links_remove], ChannelMap),
    NewChannel = erlmur_channel_store:add(FilteredMap),
    broadcast_channel(NewChannel),
    ok;
handle_message(_Session, ChannelState = #'ChannelState'{channel_id = ChannelId}) ->
    logger:info("Request to update channel ~p: ~p", [ChannelId, ChannelState]),
    % TODO: Check permissions
    ChannelMap = channel_state_to_map(ChannelState),
    FilteredMap = maps:without([links, links_add, links_remove], ChannelMap),
    UpdatedChannel = erlmur_channel_store:update(ChannelId, FilteredMap),
    broadcast_channel(UpdatedChannel),
    ok;
handle_message(#session{user = #user{id = Id}}, #'ChannelRemove'{channel_id = ChannelId}) ->
    logger:info("Request to remove channel ~p", [ChannelId]),
    % TODO: Check permissions
    erlmur_channel_store:remove(ChannelId, Id),
    broadcast_channel_removal(ChannelId),
    ok;
handle_message(Session, Message) ->
    logger:warning("Unhandled message ~p", [Message]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

broadcast_channel(Channel) ->
    AllSessionPids = pg:get_members(pg_erlmur, users),
    ChannelState = #'ChannelState'{
        channel_id = Channel#channel.id,
        parent = Channel#channel.parent_id,
        name = Channel#channel.name,
        description = Channel#channel.description,
        temporary = Channel#channel.temporary,
        position = Channel#channel.position,
        description_hash = Channel#channel.description_hash,
        max_users = Channel#channel.max_users,
        is_enter_restricted = Channel#channel.is_enter_restricted,
        can_enter = Channel#channel.can_enter
    },
    Msg = pack(ChannelState),
    lists:foreach(
        fun(SessionPid) ->
            erlmur_session:send(SessionPid, Msg)
        end,
        AllSessionPids
    ),

    case sets:is_empty(Channel#channel.links) of
        false ->
            LinkChannelState = #'ChannelState'{
                channel_id = Channel#channel.id,
                links = sets:to_list(Channel#channel.links)
            },
            LinkMsg = pack(LinkChannelState),
            lists:foreach(
                fun(SessionPid) ->
                    erlmur_session:send(SessionPid, LinkMsg)
                end,
                AllSessionPids
            );
        true ->
            ok
    end.

broadcast_channel_removal(ChannelId) ->
    AllSessionPids = pg:get_members(pg_erlmur, users),
    ChannelRemove = #'ChannelRemove'{channel_id = ChannelId},
    Msg = pack(ChannelRemove),
    lists:foreach(
        fun(SessionPid) ->
            erlmur_session:send(SessionPid, Msg)
        end,
        AllSessionPids
    ).

pack(MessageRecord) ->
    logger:debug("pack ~p", [MessageRecord]),
    case find_msg_by_record(MessageRecord) of
        {ok, Tag, _} ->
            Bin = 'Mumble_gpb':encode_msg(MessageRecord),
            encode_message(Tag, Bin);
        error ->
            error({unknown_type, MessageRecord})
    end.

unpack(<<>>, Acc) ->
    lists:reverse(Acc);
unpack(
    <<Type:16/unsigned-big-integer, Len:32/unsigned-big-integer, Msg:Len/binary, Rest/binary>>,
    Acc
) ->
    case find_msg_by_tag(Type) of
        {ok, _Type, Record} ->
            logger:debug("unpack ~p", [element(1, Record)]),
            Decoded = 'Mumble_gpb':decode_msg(Msg, element(1, Record)),
            unpack(Rest, [Decoded | Acc]);
        error ->
            logger:error("Unable to unpack Msg ~p~nLen ~p~nMsg ~p", [Type, Len, Msg]),
            unpack(Rest, Acc)
    end.

find_msg_by_record(Record) ->
    RecordName = element(1, Record),
    case lists:search(fun({_Tag, R}) -> is_record(R, RecordName) end, ?MESSAGE_TABLE) of
        {value, {Tag, _}} ->
            {ok, Tag, Record};
        false ->
            error
    end.

find_msg_by_tag(Tag) ->
    lists:foldl(
        fun
            ({T, Record}, _Acc) when T =:= Tag ->
                {ok, Tag, Record};
            (_, Acc) ->
                Acc
        end,
        error,
        ?MESSAGE_TABLE
    ).

encode_message(Type, Msg) when is_binary(Msg) ->
    Len = byte_size(Msg),
    <<Type:16/unsigned-big-integer, Len:32/unsigned-big-integer, Msg/binary>>.

channel_state_to_map(#'ChannelState'{
    channel_id = ChannelId,
    parent = Parent,
    name = Name,
    links = Links,
    description = Description,
    temporary = Temporary,
    position = Position,
    description_hash = DescriptionHash,
    links_add = LinksAdd,
    links_remove = LinksRemove,
    max_users = MaxUsers,
    is_enter_restricted = IsEnterRestricted,
    can_enter = CanEnter
}) ->
    All = [
        {id, ChannelId},
        {parent_id, Parent},
        {name, Name},
        {links, Links},
        {description, Description},
        {temporary, Temporary},
        {position, Position},
        {description_hash, DescriptionHash},
        {links_add, LinksAdd},
        {links_remove, LinksRemove},
        {max_users, MaxUsers},
        {is_enter_restricted, IsEnterRestricted},
        {can_enter, CanEnter}
    ],
    Filtered = lists:filter(fun({_Key, Value}) -> Value =/= undefined end, All),
    maps:from_list(Filtered).
