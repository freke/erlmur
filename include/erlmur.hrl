-define(MUMBLE_PROTOCOL_VERSION_MAJOR, 1).
-define(MUMBLE_PROTOCOL_VERSION_MINOR, 2).
-define(MUMBLE_PROTOCOL_VERSION_PATCH, 4).

-record(version, {major, minor, patch, release, os, os_version}).
-record(codec_version, {alpha = -2147483637, beta = 0, prefer_alpha = true, opus = true}).
-record(server_config, {
    max_bandwidth = 240000,
    allow_html = true,
    message_length = 128,
    welcome_text = <<"Welcome to Erlmur.">>,
    max_clients = 10
}).
-record(channel, {
    id,
    parent_id,
    name,
    links = sets:new(),
    description = "" :: string(),
    temporary = false :: boolean(),
    position = 0,
    description_hash,
    max_users = 10 :: non_neg_integer(),
    is_enter_restricted = false :: boolean(),
    can_enter = true :: boolean()
}).

-record(user, {
    id,
    channel_id,
    name,
    hash,
    comment,
    comment_hash,
    texture,
    texture_hash
}).
-record(registered_user, {
    id,
    name,
    last_seen,
    last_channel_id
}).
-record(ping, {
    good = 0 :: non_neg_integer(),
    late = 0 :: non_neg_integer(),
    lost = 0 :: non_neg_integer(),
    resync = 0 :: non_neg_integer()
}).
-record(stats, {
    server_ping = #ping{},
    client_ping = #ping{},
    udp_packets = 0 :: non_neg_integer(),
    tcp_packets = 0 :: non_neg_integer(),
    udp_ping_avg = 0 :: non_neg_integer(),
    udp_ping_var = 0 :: non_neg_integer(),
    tcp_ping_avg = 0 :: non_neg_integer(),
    tcp_ping_var = 0 :: non_neg_integer(),
    onlinesecs = 0 :: non_neg_integer(),
    idlesecs = 0 :: non_neg_integer(),
    from_client_tcp_packets = 0 :: non_neg_integer(),
    from_client_udp_packets = 0 :: non_neg_integer(),
    from_client_tcp_bytes = 0 :: non_neg_integer(),
    from_client_udp_bytes = 0 :: non_neg_integer()
}).

-record(udp_session, {
    ip_port :: {inet:ip_address(), inet:port_number() | pid()},
    pid :: pid()
}).

-record(session_record, {
    session_id,
    user_id,
    session_pid :: pid()
}).
-record(erlmur_session, {id}).
-record(session, {
    id,
    session_pid,
    user,
    listening_channels = [],
    mute = false,
    deaf = false,
    self_mute = false,
    self_deaf = false,
    suppress = false,
    priority_speaker = false,
    recording = false,
    listening_volume_adjust = 0,
    codec_version,
    texture_hash,
    plugin_context,
    plugin_identity,
    temporary_access_tokens,
    listening_volume_adjustment,
    client_version,
    crypto_state,
    stats = #stats{},
    address,
    udp_port,
    is_bot = false,
    use_udp_tunnel = true,
    mumble_protocol = v1_2 :: v1_2 | v1_5
}).
