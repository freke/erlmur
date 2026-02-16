-module(mumble_msg).

-moduledoc """
Message format conversion between records and maps.

This module provides bidirectional conversion between Mumble protocol records
(defined via Protocol Buffers/gpb) and Erlang maps. The map representation is
more convenient for pattern matching and manipulation.

## Conversions

Records are converted to maps with a `message_type` field indicating the type:
- `#'Version'{}` -> `#{message_type => 'Version', ...}`
- `#'Authenticate'{}` -> `#{message_type => 'Authenticate', ...}`
- etc.

## Usage

```erlang
%% Convert record to map
Map = mumble_msg:to_map(#'Version'{major = 1, minor = 2, patch = 4}).

%% Convert map back to record
Record = mumble_msg:from_map(#{message_type => 'Version', major => 1}).
```
""".

-export([to_map/1, from_map/1]).

-include("Mumble_gpb.hrl").

-doc """
Convert a Mumble protocol record to a map representation.
Input: Protocol record (e.g., #'Version'{}, #'Authenticate'{}, etc.).
Output: Map with message_type and field values.
""".
-spec to_map(tuple()) -> map().
to_map(Msg) ->
    remove_undefined(raw_to_map(Msg)).

remove_undefined(Map) ->
    maps:filter(fun(_K, V) -> V =/= undefined end, Map).

raw_to_map(#'Version'{
    version_v1 = V1,
    version_v2 = V2,
    release = Release,
    os = OS,
    os_version = OSVer
}) ->
    #{
        message_type => 'Version',
        version_v1 => V1,
        version_v2 => V2,
        release => Release,
        os => OS,
        os_version => OSVer
    };
raw_to_map(#'UDPTunnel'{packet = Packet}) ->
    #{message_type => 'UDPTunnel', packet => Packet};
raw_to_map(#'Authenticate'{
    username = Username,
    password = Password,
    tokens = Tokens,
    celt_versions = Celt,
    opus = Opus,
    client_type = ClientType
}) ->
    #{
        message_type => 'Authenticate',
        username => Username,
        password => Password,
        tokens => Tokens,
        celt_versions => Celt,
        opus => Opus,
        client_type => ClientType
    };
raw_to_map(#'Ping'{
    timestamp = TS,
    good = Good,
    late = Late,
    lost = Lost,
    resync = Resync,
    udp_packets = UdpPkts,
    tcp_packets = TcpPkts,
    udp_ping_avg = UdpAvg,
    udp_ping_var = UdpVar,
    tcp_ping_avg = TcpAvg,
    tcp_ping_var = TcpVar
}) ->
    #{
        message_type => 'Ping',
        timestamp => TS,
        good => Good,
        late => Late,
        lost => Lost,
        resync => Resync,
        udp_packets => UdpPkts,
        tcp_packets => TcpPkts,
        udp_ping_avg => UdpAvg,
        udp_ping_var => UdpVar,
        tcp_ping_avg => TcpAvg,
        tcp_ping_var => TcpVar
    };
raw_to_map(#'Reject'{type = Type, reason = Reason}) ->
    #{message_type => 'Reject', type => Type, reason => Reason};
raw_to_map(#'ServerSync'{
    session = Session,
    max_bandwidth = MaxBW,
    welcome_text = Text,
    permissions = Perms
}) ->
    #{
        message_type => 'ServerSync',
        session => Session,
        max_bandwidth => MaxBW,
        welcome_text => Text,
        permissions => Perms
    };
raw_to_map(#'ChannelRemove'{channel_id = CID}) ->
    #{message_type => 'ChannelRemove', channel_id => CID};
raw_to_map(#'ChannelState'{
    channel_id = CID,
    parent = Parent,
    name = Name,
    links = Links,
    description = Desc,
    links_add = LinksAdd,
    links_remove = LinksRemove,
    temporary = Temp,
    position = Pos,
    description_hash = DescHash,
    max_users = MaxUsers,
    is_enter_restricted = Restricted,
    can_enter = CanEnter
}) ->
    #{
        message_type => 'ChannelState',
        channel_id => CID,
        parent => Parent,
        name => Name,
        links => Links,
        description => Desc,
        links_add => LinksAdd,
        links_remove => LinksRemove,
        temporary => Temp,
        position => Pos,
        description_hash => DescHash,
        max_users => MaxUsers,
        is_enter_restricted => Restricted,
        can_enter => CanEnter
    };
raw_to_map(#'UserRemove'{
    session = Session,
    actor = Actor,
    reason = Reason,
    ban = Ban
}) ->
    #{
        message_type => 'UserRemove',
        session => Session,
        actor => Actor,
        reason => Reason,
        ban => Ban
    };
raw_to_map(#'UserState'{
    session = Session,
    actor = Actor,
    name = Name,
    user_id = UID,
    channel_id = CID,
    mute = Mute,
    deaf = Deaf,
    suppress = Suppress,
    self_mute = SelfMute,
    self_deaf = SelfDeaf,
    texture = Texture,
    plugin_context = PluginCtx,
    plugin_identity = PluginId,
    comment = Comment,
    hash = Hash,
    comment_hash = CommentHash,
    texture_hash = TextureHash,
    priority_speaker = Priority,
    recording = Recording,
    temporary_access_tokens = Tokens,
    listening_channel_add = ListenAdd,
    listening_channel_remove = ListenRemove,
    listening_volume_adjustment = ListenVol
}) ->
    #{
        message_type => 'UserState',
        session => Session,
        actor => Actor,
        name => Name,
        user_id => UID,
        channel_id => CID,
        mute => Mute,
        deaf => Deaf,
        suppress => Suppress,
        self_mute => SelfMute,
        self_deaf => SelfDeaf,
        texture => Texture,
        plugin_context => PluginCtx,
        plugin_identity => PluginId,
        comment => Comment,
        hash => Hash,
        comment_hash => CommentHash,
        texture_hash => TextureHash,
        priority_speaker => Priority,
        recording => Recording,
        temporary_access_tokens => Tokens,
        listening_channel_add => ListenAdd,
        listening_channel_remove => ListenRemove,
        listening_volume_adjustment => ListenVol
    };
raw_to_map(#'BanList'{bans = Bans, query = Query}) ->
    #{
        message_type => 'BanList',
        bans => [
            #{
                address => B#'BanList.BanEntry'.address,
                mask => B#'BanList.BanEntry'.mask,
                name => B#'BanList.BanEntry'.name,
                hash => B#'BanList.BanEntry'.hash,
                reason => B#'BanList.BanEntry'.reason,
                start => B#'BanList.BanEntry'.start,
                duration => B#'BanList.BanEntry'.duration
            }
         || B <- Bans
        ],
        query => Query
    };
raw_to_map(#'TextMessage'{
    actor = Actor,
    session = Sessions,
    channel_id = Channels,
    tree_id = Trees,
    message = Msg
}) ->
    #{
        message_type => 'TextMessage',
        actor => Actor,
        session => Sessions,
        channel_id => Channels,
        tree_id => Trees,
        message => Msg
    };
raw_to_map(#'PermissionDenied'{
    permission = Perm,
    channel_id = CID,
    session = Session,
    reason = Reason,
    type = Type,
    name = Name
}) ->
    #{
        message_type => 'PermissionDenied',
        permission => Perm,
        channel_id => CID,
        session => Session,
        reason => Reason,
        type => Type,
        name => Name
    };
raw_to_map(#'ACL'{
    channel_id = CID,
    inherit_acls = Inherit,
    groups = Groups,
    acls = ACLs,
    query = Query
}) ->
    #{
        message_type => 'ACL',
        channel_id => CID,
        inherit_acls => Inherit,
        groups => [
            #{
                name => G#'ACL.ChanGroup'.name,
                inherited => G#'ACL.ChanGroup'.inherited,
                inherit => G#'ACL.ChanGroup'.inherit,
                inheritable => G#'ACL.ChanGroup'.inheritable,
                add => G#'ACL.ChanGroup'.add,
                remove => G#'ACL.ChanGroup'.remove,
                inherited_members => G#'ACL.ChanGroup'.inherited_members
            }
         || G <- Groups
        ],
        acls => [
            #{
                apply_here => A#'ACL.ChanACL'.apply_here,
                apply_subs => A#'ACL.ChanACL'.apply_subs,
                inherited => A#'ACL.ChanACL'.inherited,
                user_id => A#'ACL.ChanACL'.user_id,
                group => A#'ACL.ChanACL'.group,
                grant => A#'ACL.ChanACL'.grant,
                deny => A#'ACL.ChanACL'.deny
            }
         || A <- ACLs
        ],
        query => Query
    };
raw_to_map(#'QueryUsers'{ids = IDs, names = Names}) ->
    #{message_type => 'QueryUsers', ids => IDs, names => Names};
raw_to_map(#'CryptSetup'{
    key = Key,
    client_nonce = CNonce,
    server_nonce = SNonce
}) ->
    #{
        message_type => 'CryptSetup',
        key => Key,
        client_nonce => CNonce,
        server_nonce => SNonce
    };
raw_to_map(#'ContextActionModify'{
    action = Action,
    text = Text,
    context = Ctx,
    operation = Op
}) ->
    #{
        message_type => 'ContextActionModify',
        action => Action,
        text => Text,
        context => Ctx,
        operation => Op
    };
raw_to_map(#'ContextAction'{
    session = Session,
    channel_id = CID,
    action = Action
}) ->
    #{
        message_type => 'ContextAction',
        session => Session,
        channel_id => CID,
        action => Action
    };
raw_to_map(#'UserList'{users = Users}) ->
    #{
        message_type => 'UserList',
        users => [
            #{
                user_id => U#'UserList.User'.user_id,
                name => U#'UserList.User'.name,
                last_seen => U#'UserList.User'.last_seen,
                last_channel => U#'UserList.User'.last_channel
            }
         || U <- Users
        ]
    };
raw_to_map(#'VoiceTarget'{id = ID, targets = Targets}) ->
    #{
        message_type => 'VoiceTarget',
        id => ID,
        targets => [
            #{
                session => T#'VoiceTarget.Target'.session,
                channel_id => T#'VoiceTarget.Target'.channel_id,
                group => T#'VoiceTarget.Target'.group,
                links => T#'VoiceTarget.Target'.links,
                children => T#'VoiceTarget.Target'.children
            }
         || T <- Targets
        ]
    };
raw_to_map(#'PermissionQuery'{
    channel_id = CID,
    permissions = Perms,
    flush = Flush
}) ->
    #{
        message_type => 'PermissionQuery',
        channel_id => CID,
        permissions => Perms,
        flush => Flush
    };
raw_to_map(#'CodecVersion'{
    alpha = Alpha,
    beta = Beta,
    prefer_alpha = PreferAlpha,
    opus = Opus
}) ->
    #{
        message_type => 'CodecVersion',
        alpha => Alpha,
        beta => Beta,
        prefer_alpha => PreferAlpha,
        opus => Opus
    };
raw_to_map(#'UserStats'{
    session = Session,
    stats_only = StatsOnly,
    certificates = Certs,
    from_client = FromClient,
    from_server = FromServer,
    udp_packets = UdpPkts,
    tcp_packets = TcpPkts,
    udp_ping_avg = UdpAvg,
    udp_ping_var = UdpVar,
    tcp_ping_avg = TcpAvg,
    tcp_ping_var = TcpVar,
    version = Version,
    celt_versions = Celt,
    address = Addr,
    bandwidth = BW,
    onlinesecs = Online,
    idlesecs = Idle,
    strong_certificate = Strong,
    opus = Opus,
    rolling_stats = Rolling
}) ->
    #{
        message_type => 'UserStats',
        session => Session,
        stats_only => StatsOnly,
        certificates => Certs,
        from_client => FromClient,
        from_server => FromServer,
        udp_packets => UdpPkts,
        tcp_packets => TcpPkts,
        udp_ping_avg => UdpAvg,
        udp_ping_var => UdpVar,
        tcp_ping_avg => TcpAvg,
        tcp_ping_var => TcpVar,
        version => Version,
        celt_versions => Celt,
        address => Addr,
        bandwidth => BW,
        onlinesecs => Online,
        idlesecs => Idle,
        strong_certificate => Strong,
        opus => Opus,
        rolling_stats => Rolling
    };
raw_to_map(#'RequestBlob'{
    session_texture = SessionTexture,
    session_comment = SessionComment,
    channel_description = ChannelDesc
}) ->
    #{
        message_type => 'RequestBlob',
        session_texture => SessionTexture,
        session_comment => SessionComment,
        channel_description => ChannelDesc
    };
raw_to_map(#'ServerConfig'{
    max_bandwidth = MaxBW,
    welcome_text = Text,
    allow_html = AllowHTML,
    message_length = MsgLen,
    image_message_length = ImgLen,
    max_users = MaxUsers,
    recording_allowed = RecAllowed
}) ->
    #{
        message_type => 'ServerConfig',
        max_bandwidth => MaxBW,
        welcome_text => Text,
        allow_html => AllowHTML,
        message_length => MsgLen,
        image_message_length => ImgLen,
        max_users => MaxUsers,
        recording_allowed => RecAllowed
    };
raw_to_map(#'SuggestConfig'{
    version_v1 = V1,
    version_v2 = V2,
    positional = Pos,
    push_to_talk = PTT
}) ->
    #{
        message_type => 'SuggestConfig',
        version_v1 => V1,
        version_v2 => V2,
        positional => Pos,
        push_to_talk => PTT
    };
raw_to_map(#'PluginDataTransmission'{
    senderSession = Sender,
    receiverSessions = Receivers,
    data = Data,
    dataID = DID
}) ->
    #{
        message_type => 'PluginDataTransmission',
        senderSession => Sender,
        receiverSessions => Receivers,
        data => Data,
        dataID => DID
    }.

-doc """
Convert a map representation to a Mumble protocol record.
Input: Map with message_type and field values.
Output: Protocol record (e.g., #'Version'{}, #'Authenticate'{}, etc.).
""".
-spec from_map(map()) -> tuple().
from_map(#{message_type := 'Version'} = Map) ->
    #'Version'{
        version_v1 = maps:get(version_v1, Map, undefined),
        version_v2 = maps:get(version_v2, Map, undefined),
        release = maps:get(release, Map, undefined),
        os = maps:get(os, Map, undefined),
        os_version = maps:get(os_version, Map, undefined)
    };
from_map(#{message_type := 'UDPTunnel'} = Map) ->
    #'UDPTunnel'{packet = maps:get(packet, Map)};
from_map(#{message_type := 'Authenticate'} = Map) ->
    #'Authenticate'{
        username = maps:get(username, Map, undefined),
        password = maps:get(password, Map, undefined),
        tokens = maps:get(tokens, Map, []),
        celt_versions = maps:get(celt_versions, Map, []),
        opus = maps:get(opus, Map, false),
        client_type = maps:get(client_type, Map, 0)
    };
from_map(#{message_type := 'Ping'} = Map) ->
    #'Ping'{
        timestamp = maps:get(timestamp, Map, erlang:system_time(second)),
        good = maps:get(good, Map, undefined),
        late = maps:get(late, Map, undefined),
        lost = maps:get(lost, Map, undefined),
        resync = maps:get(resync, Map, undefined),
        udp_packets = maps:get(udp_packets, Map, undefined),
        tcp_packets = maps:get(tcp_packets, Map, undefined),
        udp_ping_avg = maps:get(udp_ping_avg, Map, undefined),
        udp_ping_var = maps:get(udp_ping_var, Map, undefined),
        tcp_ping_avg = maps:get(tcp_ping_avg, Map, undefined),
        tcp_ping_var = maps:get(tcp_ping_var, Map, undefined)
    };
from_map(#{message_type := 'Reject'} = Map) ->
    #'Reject'{
        type = maps:get(type, Map, undefined),
        reason = maps:get(reason, Map, undefined)
    };
from_map(#{message_type := 'ServerSync'} = Map) ->
    #'ServerSync'{
        session = maps:get(session, Map, undefined),
        max_bandwidth = maps:get(max_bandwidth, Map, undefined),
        welcome_text = maps:get(welcome_text, Map, undefined),
        permissions = maps:get(permissions, Map, undefined)
    };
from_map(#{message_type := 'ChannelRemove'} = Map) ->
    #'ChannelRemove'{channel_id = maps:get(channel_id, Map)};
from_map(#{message_type := 'ChannelState'} = Map) ->
    #'ChannelState'{
        channel_id = maps:get(channel_id, Map, undefined),
        parent = maps:get(parent, Map, undefined),
        name = maps:get(name, Map, undefined),
        links = maps:get(links, Map, []),
        description = maps:get(description, Map, undefined),
        links_add = maps:get(links_add, Map, []),
        links_remove = maps:get(links_remove, Map, []),
        temporary = maps:get(temporary, Map, false),
        position = maps:get(position, Map, 0),
        description_hash = maps:get(description_hash, Map, undefined),
        max_users = maps:get(max_users, Map, undefined),
        is_enter_restricted = maps:get(is_enter_restricted, Map, undefined),
        can_enter = maps:get(can_enter, Map, undefined)
    };
from_map(#{message_type := 'UserRemove'} = Map) ->
    #'UserRemove'{
        session = maps:get(session, Map),
        actor = maps:get(actor, Map, undefined),
        reason = maps:get(reason, Map, undefined),
        ban = maps:get(ban, Map, undefined)
    };
from_map(#{message_type := 'UserState'} = Map) ->
    #'UserState'{
        session = maps:get(session, Map, undefined),
        actor = maps:get(actor, Map, undefined),
        name = maps:get(name, Map, undefined),
        user_id = maps:get(user_id, Map, undefined),
        channel_id = maps:get(channel_id, Map, undefined),
        mute = maps:get(mute, Map, undefined),
        deaf = maps:get(deaf, Map, undefined),
        suppress = maps:get(suppress, Map, undefined),
        self_mute = maps:get(self_mute, Map, undefined),
        self_deaf = maps:get(self_deaf, Map, undefined),
        texture = maps:get(texture, Map, undefined),
        plugin_context = maps:get(plugin_context, Map, undefined),
        plugin_identity = maps:get(plugin_identity, Map, undefined),
        comment = maps:get(comment, Map, undefined),
        hash = maps:get(hash, Map, undefined),
        comment_hash = maps:get(comment_hash, Map, undefined),
        texture_hash = maps:get(texture_hash, Map, undefined),
        priority_speaker = maps:get(priority_speaker, Map, undefined),
        recording = maps:get(recording, Map, undefined),
        temporary_access_tokens = maps:get(temporary_access_tokens, Map, []),
        listening_channel_add = maps:get(listening_channel_add, Map, []),
        listening_channel_remove = maps:get(listening_channel_remove, Map, []),
        listening_volume_adjustment = maps:get(listening_volume_adjustment, Map, [])
    };
from_map(#{message_type := 'BanList'} = Map) ->
    #'BanList'{
        bans = [
            #'BanList.BanEntry'{
                address = maps:get(address, B),
                mask = maps:get(mask, B),
                name = maps:get(name, B, undefined),
                hash = maps:get(hash, B, undefined),
                reason = maps:get(reason, B, undefined),
                start = maps:get(start, B, undefined),
                duration = maps:get(duration, B, undefined)
            }
         || B <- maps:get(bans, Map, [])
        ],
        query = maps:get(query, Map, false)
    };
from_map(#{message_type := 'TextMessage'} = Map) ->
    #'TextMessage'{
        actor = maps:get(actor, Map, undefined),
        session = maps:get(session, Map, []),
        channel_id = maps:get(channel_id, Map, []),
        tree_id = maps:get(tree_id, Map, []),
        message = maps:get(message, Map)
    };
from_map(#{message_type := 'PermissionDenied'} = Map) ->
    #'PermissionDenied'{
        permission = maps:get(permission, Map, undefined),
        channel_id = maps:get(channel_id, Map, undefined),
        session = maps:get(session, Map, undefined),
        reason = maps:get(reason, Map, undefined),
        type = maps:get(type, Map, undefined),
        name = maps:get(name, Map, undefined)
    };
from_map(#{message_type := 'ACL'} = Map) ->
    #'ACL'{
        channel_id = maps:get(channel_id, Map),
        inherit_acls = maps:get(inherit_acls, Map, true),
        groups = [
            #'ACL.ChanGroup'{
                name = maps:get(name, G),
                inherited = maps:get(inherited, G, true),
                inherit = maps:get(inherit, G, true),
                inheritable = maps:get(inheritable, G, true),
                add = maps:get(add, G, []),
                remove = maps:get(remove, G, []),
                inherited_members = maps:get(inherited_members, G, [])
            }
         || G <- maps:get(groups, Map, [])
        ],
        acls = [
            #'ACL.ChanACL'{
                apply_here = maps:get(apply_here, A, true),
                apply_subs = maps:get(apply_subs, A, true),
                inherited = maps:get(inherited, A, true),
                user_id = maps:get(user_id, A, undefined),
                group = maps:get(group, A, undefined),
                grant = maps:get(grant, A, undefined),
                deny = maps:get(deny, A, undefined)
            }
         || A <- maps:get(acls, Map, [])
        ],
        query = maps:get(query, Map, false)
    };
from_map(#{message_type := 'QueryUsers'} = Map) ->
    #'QueryUsers'{
        ids = maps:get(ids, Map, []),
        names = maps:get(names, Map, [])
    };
from_map(#{message_type := 'CryptSetup'} = Map) ->
    #'CryptSetup'{
        key = maps:get(key, Map, undefined),
        client_nonce = maps:get(client_nonce, Map, undefined),
        server_nonce = maps:get(server_nonce, Map, undefined)
    };
from_map(#{message_type := 'ContextActionModify'} = Map) ->
    #'ContextActionModify'{
        action = maps:get(action, Map),
        text = maps:get(text, Map, undefined),
        context = maps:get(context, Map, undefined),
        operation = maps:get(operation, Map, undefined)
    };
from_map(#{message_type := 'ContextAction'} = Map) ->
    #'ContextAction'{
        session = maps:get(session, Map, undefined),
        channel_id = maps:get(channel_id, Map, undefined),
        action = maps:get(action, Map)
    };
from_map(#{message_type := 'UserList'} = Map) ->
    #'UserList'{
        users = [
            #'UserList.User'{
                user_id = maps:get(user_id, U),
                name = maps:get(name, U, undefined),
                last_seen = maps:get(last_seen, U, undefined),
                last_channel = maps:get(last_channel, U, undefined)
            }
         || U <- maps:get(users, Map, [])
        ]
    };
from_map(#{message_type := 'VoiceTarget'} = Map) ->
    #'VoiceTarget'{
        id = maps:get(id, Map, undefined),
        targets = [
            #'VoiceTarget.Target'{
                session = maps:get(session, T, []),
                channel_id = maps:get(channel_id, T, undefined),
                group = maps:get(group, T, undefined),
                links = maps:get(links, T, false),
                children = maps:get(children, T, false)
            }
         || T <- maps:get(targets, Map, [])
        ]
    };
from_map(#{message_type := 'PermissionQuery'} = Map) ->
    #'PermissionQuery'{
        channel_id = maps:get(channel_id, Map, undefined),
        permissions = maps:get(permissions, Map, undefined),
        flush = maps:get(flush, Map, false)
    };
from_map(#{message_type := 'CodecVersion'} = Map) ->
    #'CodecVersion'{
        alpha = maps:get(alpha, Map),
        beta = maps:get(beta, Map),
        prefer_alpha = maps:get(prefer_alpha, Map, true),
        opus = maps:get(opus, Map, false)
    };
from_map(#{message_type := 'UserStats'} = Map) ->
    #'UserStats'{
        session = maps:get(session, Map, undefined),
        stats_only = maps:get(stats_only, Map, false),
        certificates = maps:get(certificates, Map, []),
        from_client = maps:get(from_client, Map, undefined),
        from_server = maps:get(from_server, Map, undefined),
        udp_packets = maps:get(udp_packets, Map, undefined),
        tcp_packets = maps:get(tcp_packets, Map, undefined),
        udp_ping_avg = maps:get(udp_ping_avg, Map, undefined),
        udp_ping_var = maps:get(udp_ping_var, Map, undefined),
        tcp_ping_avg = maps:get(tcp_ping_avg, Map, undefined),
        tcp_ping_var = maps:get(tcp_ping_var, Map, undefined),
        version = maps:get(version, Map, undefined),
        celt_versions = maps:get(celt_versions, Map, []),
        address = maps:get(address, Map, undefined),
        bandwidth = maps:get(bandwidth, Map, undefined),
        onlinesecs = maps:get(onlinesecs, Map, undefined),
        idlesecs = maps:get(idlesecs, Map, undefined),
        strong_certificate = maps:get(strong_certificate, Map, false),
        opus = maps:get(opus, Map, false),
        rolling_stats = maps:get(rolling_stats, Map, undefined)
    };
from_map(#{message_type := 'RequestBlob'} = Map) ->
    #'RequestBlob'{
        session_texture = maps:get(session_texture, Map, []),
        session_comment = maps:get(session_comment, Map, []),
        channel_description = maps:get(channel_description, Map, [])
    };
from_map(#{message_type := 'ServerConfig'} = Map) ->
    #'ServerConfig'{
        max_bandwidth = maps:get(max_bandwidth, Map, undefined),
        welcome_text = maps:get(welcome_text, Map, undefined),
        allow_html = maps:get(allow_html, Map, undefined),
        message_length = maps:get(message_length, Map, undefined),
        image_message_length = maps:get(image_message_length, Map, undefined),
        max_users = maps:get(max_users, Map, undefined),
        recording_allowed = maps:get(recording_allowed, Map, undefined)
    };
from_map(#{message_type := 'SuggestConfig'} = Map) ->
    #'SuggestConfig'{
        version_v1 = maps:get(version_v1, Map, undefined),
        version_v2 = maps:get(version_v2, Map, undefined),
        positional = maps:get(positional, Map, undefined),
        push_to_talk = maps:get(push_to_talk, Map, undefined)
    };
from_map(#{message_type := 'PluginDataTransmission'} = Map) ->
    #'PluginDataTransmission'{
        senderSession = maps:get(senderSession, Map, undefined),
        receiverSessions = maps:get(receiverSessions, Map, []),
        data = maps:get(data, Map, undefined),
        dataID = maps:get(dataID, Map, undefined)
    }.
