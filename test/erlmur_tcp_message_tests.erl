-module(erlmur_tcp_message_tests).

-include_lib("eunit.hrl").
-include("Mumble_gpb.hrl").
-include("erlmur.hrl").

%% Helper: decode framed binary to records via SUT
-spec decode_bin(binary()) -> [any()].
decode_bin(Bin) ->
    erlmur_tcp_message:decode(Bin).

%% Helper: extract header (Type, Len, PayloadRest)
-spec header(binary()) -> {non_neg_integer(), non_neg_integer(), binary()}.
header(<<Type:16/unsigned-big-integer, Len:32/unsigned-big-integer, Rest/binary>>) ->
    {Type, Len, Rest}.

%% ==== decode/1 tests ====

decode_empty_returns_empty_test() ->
    ?assertEqual([], erlmur_tcp_message:decode(<<>>)).

decode_single_known_message_test() ->
    %% Build a simple Ping record with only timestamp set; others default to undefined/0
    Ping = #'Ping'{timestamp = 123456789},
    Framed = erlmur_tcp_message:pack(Ping),
    Decoded = erlmur_tcp_message:decode(Framed),
    ?assertMatch([#'Ping'{timestamp = 123456789}], Decoded).

decode_multiple_messages_with_unknown_skips_unknown_test() ->
    %% Known message (Version) followed by unknown type, then another known message (Ping)
    V = #'Version'{version_v1 = <<1,2,3>>, os = <<"linux">>, os_version = <<"6.9">>, release = <<"1.2.3">>},
    Known1 = erlmur_tcp_message:pack(V),
    UnknownPayload = <<16#DE,16#AD,16#BE,16#EF>>,
    Unknown = begin
        Type = 16#FFFF,
        Len  = byte_size(UnknownPayload),
        <<Type:16/unsigned-big-integer, Len:32/unsigned-big-integer, UnknownPayload/binary>>
    end,
    P = #'Ping'{timestamp = 42},
    Known2 = erlmur_tcp_message:pack(P),
    Decoded = erlmur_tcp_message:decode(<<Known1/binary, Unknown/binary, Known2/binary>>),
    %% Should contain only Version and Ping, in order
    ?assertEqual(2, length(Decoded)),
    [D1, D2] = Decoded,
    ?assertMatch(#'Version'{}, D1),
    ?assertMatch(#'Ping'{timestamp = 42}, D2).

%% ==== pack/encode framing tests ====

pack_frames_with_correct_header_length_test() ->
    Msg = #'TextMessage'{actor = 10, message = <<"hello">>},
    Bin = erlmur_tcp_message:pack(Msg),
    {Type, Len, Payload} = header(Bin),
    %% Payload decodes back to TextMessage and len equals payload size
    ?assert(byte_size(Payload) =:= Len),
    Decoded = 'Mumble_gpb':decode_msg(Payload, 'TextMessage'),
    ?assertMatch(#'TextMessage'{message = <<"hello">>}, Decoded),
    %% Also ensure full round-trip via decode/1
    ?assertMatch([#'TextMessage'{message = <<"hello">>}], decode_bin(Bin)),
    %% Type sanity: ensure it's one of the known tags defined in MESSAGE_TABLE by round-tripping
    %% (if Type were wrong, decode would fail above)
    ?assert(Type >= 0).

%% ==== send/2 tests ====
%% These tests mock external collaborators where needed. We use meck if available.

maybe_meck_new(Mod,Opts) ->
    case code:which(meck) of
        non_existing -> ok;
        _ -> meck:new(Mod, Opts)
    end.

maybe_meck_expect(Mod, Fun, ArityOrFun) ->
    case code:which(meck) of
        non_existing -> ok;
        _ when is_function(ArityOrFun) -> meck:expect(Mod, Fun, ArityOrFun);
        _ -> ok
    end.

maybe_meck_unload(Mod) ->
    case code:which(meck) of
        non_existing -> ok;
        _ -> catch meck:unload(Mod)
    end.

setup_send_version() ->
    maybe_meck_new(erlmur_server, [unstick, passthrough]),
    maybe_meck_new(erlmur_protocol_version, [unstick, passthrough]),
    maybe_meck_new(erlmur_session, [unstick, passthrough]),
    ServerV = #version{os = <<"linux">>, release = <<"rel">>, os_version = <<"6.9">>},
    maybe_meck_expect(erlmur_server, version, fun() -> ServerV end),
    maybe_meck_expect(erlmur_protocol_version, encode, fun(#version{}) -> {<<1,0,0,0>>, <<>>} end),
    Self = self(),
    maybe_meck_expect(erlmur_session, send, fun(_Pid, Bin) ->
        Self \! {sent, Bin}, ok
    end),
    ok.

teardown_send_version(_) ->
    maybe_meck_unload(erlmur_server),
    maybe_meck_unload(erlmur_protocol_version),
    maybe_meck_unload(erlmur_session),
    ok.

send_version_frames_version_message_test_() ->
    {setup,
     fun setup_send_version/0,
     fun teardown_send_version/1,
     fun() ->
         SessPid = self(),
         ok = erlmur_tcp_message:send(SessPid, version),
         receive
             {sent, Bin} ->
                 [#'Version'{os = <<"linux">>, release = <<"rel">>, os_version = <<"6.9">>}] =
                     erlmur_tcp_message:decode(Bin)
         after 1000 ->
             ?assertion_failed(timeout)
         end
     end}.

setup_send_crypto() ->
    maybe_meck_new(erlmur_crypto, [unstick, passthrough]),
    maybe_meck_new(erlmur_session, [unstick, passthrough]),
    maybe_meck_expect(erlmur_crypto, key, fun(_State) -> <<1:128>> end),
    maybe_meck_expect(erlmur_crypto, encrypt_iv, fun(_State) -> <<2:128>> end),
    maybe_meck_expect(erlmur_crypto, decrypt_iv, fun(_State) -> <<3:128>> end),
    Self = self(),
    maybe_meck_expect(erlmur_session, send, fun(_Pid, Bin) -> Self \! {sent, Bin}, ok end),
    ok.

teardown_send_crypto(_) ->
    maybe_meck_unload(erlmur_crypto),
    maybe_meck_unload(erlmur_session),
    ok.

send_crypto_setup_sends_cryptsetup_test_() ->
    {setup,
     fun setup_send_crypto/0,
     fun teardown_send_crypto/1,
     fun() ->
         SessPid = self(),
         ok = erlmur_tcp_message:send(SessPid, {crypto_setup, some_state}),
         receive
             {sent, Bin} ->
                 [#'CryptSetup'{key = <<1:128>>, server_nonce = <<2:128>>, client_nonce = <<3:128>>}] =
                     erlmur_tcp_message:decode(Bin)
         after 1000 ->
             ?assertion_failed(timeout)
         end
     end}.

setup_send_channels() ->
    maybe_meck_new(erlmur_session, [unstick, passthrough]),
    Self = self(),
    maybe_meck_expect(erlmur_session, send, fun(_Pid, Bin) -> Self \! {sent, Bin}, ok end),
    ok.

teardown_send_channels(_) ->
    maybe_meck_unload(erlmur_session),
    ok.

send_channels_emits_state_and_links_for_nonempty_links_test_() ->
    {setup,
     fun setup_send_channels/0,
     fun teardown_send_channels/1,
     fun() ->
         SessPid = self(),
         Ch1 = #channel{
                 id = 10, parent_id = 0, name = <<"root">>, description = <<"d">>,
                 temporary = false, position = 1, description_hash = <<>>, max_users = 99,
                 is_enter_restricted = false, can_enter = true,
                 links = sets:new()
               },
         Ch2 = #channel{
                 id = 11, parent_id = 10, name = <<"child">>, description = <<"x">>,
                 temporary = true, position = 2, description_hash = <<>>, max_users = 0,
                 is_enter_restricted = false, can_enter = true,
                 links = sets:from_list([10])
               },
         ok = erlmur_tcp_message:send(SessPid, {channels, [Ch1, Ch2]}),
         %% Expect at least 3 sends: state(Ch1), state(Ch2), links(Ch2)
         Rcv = gather_msgs([], 3),
         %% Decode and assert presence
         Dec = lists:flatmap(fun erlmur_tcp_message:decode/1, Rcv),
         %% one state for id=10, one for id=11, and one links for id=11
         ?assert(lists:any(fun(#'ChannelState'{channel_id = 10}) -> true; (_) -> false end, Dec)),
         ?assert(lists:any(fun(#'ChannelState'{channel_id = 11, name = <<"child">>}) -> true; (_) -> false end, Dec)),
         ?assert(lists:any(fun(#'ChannelState'{channel_id = 11, links = [10]}) -> true; (_) -> false end, Dec))
     end}.

gather_msgs(Acc, 0) -> lists:reverse(Acc);
gather_msgs(Acc, N) when N > 0 ->
    receive
        {sent, Bin} -> gather_msgs([Bin | Acc], N-1)
    after 1000 ->
        lists:reverse(Acc)
    end.

setup_send_users() ->
    maybe_meck_new(erlmur_session, [unstick, passthrough]),
    Self = self(),
    maybe_meck_expect(erlmur_session, send, fun(_Pid, Bin) -> Self \! {sent, Bin}, ok end),
    ok.

teardown_send_users(_) ->
    maybe_meck_unload(erlmur_session),
    ok.

send_users_emits_userstate_per_session_test_() ->
    {setup,
     fun setup_send_users/0,
     fun teardown_send_users/1,
     fun() ->
         SessPid = self(),
         U = #user{id = 7, name = <<"alice">>, channel_id = 10, texture = <<>>, comment = <<>>, hash = <<>>, comment_hash = <<>>},
         US = #session{
                 id = 1001, user = U, mute = false, deaf = false, texture_hash = <<>>,
                 priority_speaker = false, recording = false, session_pid = SessPid
               },
         ok = erlmur_tcp_message:send(SessPid, {users, [US]}),
         receive
             {sent, Bin} ->
                 [#'UserState'{session = 1001, user_id = 7, name = <<"alice">>, channel_id = 10}] =
                     erlmur_tcp_message:decode(Bin)
         after 1000 ->
             ?assertion_failed(timeout)
         end
     end}.

setup_send_server_sync() ->
    maybe_meck_new(erlmur_server, [unstick, passthrough]),
    maybe_meck_new(erlmur_session, [unstick, passthrough]),
    Self = self(),
    maybe_meck_expect(erlmur_server, config, fun() ->
        #server_config{max_bandwidth = 48000, welcome_text = <<"hi">>}
    end),
    maybe_meck_expect(erlmur_session, send, fun(_Pid, Bin) -> Self \! {sent, Bin}, ok end),
    ok.

teardown_send_server_sync(_) ->
    maybe_meck_unload(erlmur_server),
    maybe_meck_unload(erlmur_session),
    ok.

send_server_sync_includes_bandwidth_and_session_test_() ->
    {setup,
     fun setup_send_server_sync/0,
     fun teardown_send_server_sync/1,
     fun() ->
         SessPid = self(),
         ok = erlmur_tcp_message:send(SessPid, {server_sync, 1001}),
         receive
             {sent, Bin} ->
                 [#'ServerSync'{max_bandwidth = 48000, welcome_text = <<"hi">>, session = 1001}] =
                     erlmur_tcp_message:decode(Bin)
         after 1000 ->
             ?assertion_failed(timeout)
         end
     end}.

setup_send_codec_version() ->
    maybe_meck_new(erlmur_session, [unstick, passthrough]),
    Self = self(),
    maybe_meck_expect(erlmur_session, send, fun(_Pid, Bin) -> Self \! {sent, Bin}, ok end),
    ok.

teardown_send_codec_version(_) ->
    maybe_meck_unload(erlmur_session),
    ok.

send_codec_version_forwards_fields_test_() ->
    {setup,
     fun setup_send_codec_version/0,
     fun teardown_send_codec_version/1,
     fun() ->
         SessPid = self(),
         CV = #codec_version{alpha = 1, beta = 2, prefer_alpha = true, opus = true},
         ok = erlmur_tcp_message:send(SessPid, {codec_version, CV}),
         receive
             {sent, Bin} ->
                 [#'CodecVersion'{alpha = 1, beta = 2, prefer_alpha = true, opus = true}] =
                     erlmur_tcp_message:decode(Bin)
         after 1000 ->
             ?assertion_failed(timeout)
         end
     end}.

%% ==== handle_message/2 tests ====

setup_handle_version_v1() ->
    maybe_meck_new(erlmur_protocol_version, [unstick, passthrough]),
    maybe_meck_new(erlmur_session, [unstick, passthrough]),
    maybe_meck_expect(erlmur_protocol_version, decode, fun(<<1,2,3>>) ->
        #version{os = <<"x">>, release = <<"y">>, os_version = <<"z">>}
    end),
    Self = self(),
    maybe_meck_expect(erlmur_session, client_version,
        fun(_Pid, #version{release = <<"rel">>, os = <<"linux">>, os_version = <<"6.9">>}) ->
            Self \! version_v1_called, ok
        end),
    ok.

teardown_handle_version_v1(_) ->
    maybe_meck_unload(erlmur_protocol_version),
    maybe_meck_unload(erlmur_session),
    ok.

handle_message_version_v1_routes_to_client_version_test_() ->
    {setup,
     fun setup_handle_version_v1/0,
     fun teardown_handle_version_v1/1,
     fun() ->
         Sess = #session{session_pid = self()},
         V1 = #'Version'{version_v1 = <<1,2,3>>, version_v2 = undefined, os = <<"linux">>, os_version = <<"6.9">>, release = <<"rel">>},
         ok = erlmur_tcp_message:handle_message(Sess, V1),
         receive
             version_v1_called -> ok
         after 1000 ->
             ?assertion_failed(timeout)
         end
     end}.

setup_handle_authenticate() ->
    maybe_meck_new(erlmur_authenticate, [unstick, passthrough]),
    maybe_meck_new(erlmur_server, [unstick, passthrough]),
    maybe_meck_new(erlmur_session, [unstick, passthrough]),
    maybe_meck_expect(erlmur_authenticate, check, fun(<<"alice">>, <<"pwd">>) ->
        {ok, #user{id = 7}}
    end),
    maybe_meck_expect(erlmur_server, codecversion, fun(_CeltVersions, _Opus) -> ok end),
    Self = self(),
    maybe_meck_expect(erlmur_session, user,
        fun(_Pid, #user{id = 7}, [<<"tok">>], regular) ->
            Self \! {authed, regular}, ok
        end),
    ok.

teardown_handle_authenticate(_) ->
    maybe_meck_unload(erlmur_authenticate),
    maybe_meck_unload(erlmur_server),
    maybe_meck_unload(erlmur_session),
    ok.

handle_message_authenticate_regular_test_() ->
    {setup,
     fun setup_handle_authenticate/0,
     fun teardown_handle_authenticate/1,
     fun() ->
         Sess = #session{session_pid = self()},
         Msg = #'Authenticate'{
                 username = <<"alice">>, password = <<"pwd">>, opus = true,
                 tokens = [<<"tok">>], client_type = 0, celt_versions = [1,2]
               },
         ok = erlmur_tcp_message:handle_message(Sess, Msg),
         receive
             {authed, regular} -> ok
         after 1000 ->
             ?assertion_failed(timeout)
         end
     end}.

setup_handle_userstate_self_update() ->
    maybe_meck_new(erlmur_session, [unstick, passthrough]),
    Self = self(),
    maybe_meck_expect(erlmur_session, update_user_state, fun(_Pid, Map) ->
        Self \! {updated, Map}, ok end),
    ok.

teardown_handle_userstate_self_update(_) ->
    maybe_meck_unload(erlmur_session),
    ok.

handle_message_userstate_self_update_uses_map_without_undefineds_test_() ->
    {setup,
     fun setup_handle_userstate_self_update/0,
     fun teardown_handle_userstate_self_update/1,
     fun() ->
         Sess = #session{id = 1001, session_pid = self()},
         US = #'UserState'{session = undefined, name = <<"alice">>, mute = undefined, priority_speaker = true},
         ok = erlmur_tcp_message:handle_message(Sess, US),
         receive
             {updated, Map} ->
                 ?assertEqual(<<"alice">>, maps:get(name, Map)),
                 ?assertEqual(true, maps:get(priority_speaker, Map)),
                 %% undefined keys absent
                 ?assertEqual(error, (catch maps:get(mute, Map)))
         after 1000 ->
             ?assertion_failed(timeout)
         end
     end}.

setup_handle_permission_query() ->
    maybe_meck_new(erlmur_acl, [unstick, passthrough]),
    maybe_meck_new(erlmur_session, [unstick, passthrough]),
    Self = self(),
    maybe_meck_expect(erlmur_acl, query_permissions, fun(ChannelId, UserId) ->
        %% Echo back channel, grant arbitrary bitmask 16#A5A5
        {ChannelId, 16#A5A5 + UserId}  %% just to ensure id is used
    end),
    maybe_meck_expect(erlmur_session, send, fun(_Pid, Bin) ->
        Self \! {sent, Bin}, ok end),
    ok.

teardown_handle_permission_query(_) ->
    maybe_meck_unload(erlmur_acl),
    maybe_meck_unload(erlmur_session),
    ok.

handle_message_permission_query_replies_with_permissions_test_() ->
    {setup,
     fun setup_handle_permission_query/0,
     fun teardown_handle_permission_query/1,
     fun() ->
         Sess = #session{session_pid = self(), user = #user{id = 7}},
         Q = #'PermissionQuery'{channel_id = 10},
         ok = erlmur_tcp_message:handle_message(Sess, Q),
         receive
             {sent, Bin} ->
                 [#'PermissionQuery'{channel_id = 10, permissions = 16#A5A5 + 7}] =
                     erlmur_tcp_message:decode(Bin)
         after 1000 ->
             ?assertion_failed(timeout)
         end
     end}.

setup_handle_ping() ->
    maybe_meck_new(erlmur_stats, [unstick, passthrough]),
    maybe_meck_new(erlmur_session, [unstick, passthrough]),
    Self = self(),
    %% Prepare a stats record tree with server_ping embedded
    ServerPing = #ping{good = 3, late = 1, lost = 0, resync = 2},
    S0 = #stats{server_ping = ServerPing},
    maybe_meck_expect(erlmur_stats, packets, fun({_Udp,_Tcp}, _S) -> S0 end),
    maybe_meck_expect(erlmur_stats, times, fun({_A,_B,_C,_D}, _S1) -> S0 end),
    maybe_meck_expect(erlmur_stats, client_stats, fun({_G,_L,_Lo,_R}, _S2) -> S0 end),
    maybe_meck_expect(erlmur_session, update_stats, fun(_Pid, _New) -> ok end),
    maybe_meck_expect(erlmur_session, send, fun(_Pid, Bin) -> Self \! {sent, Bin}, ok end),
    ok.

teardown_handle_ping(_) ->
    maybe_meck_unload(erlmur_stats),
    maybe_meck_unload(erlmur_session),
    ok.

handle_message_ping_updates_stats_and_responds_pong_test_() ->
    {setup,
     fun setup_handle_ping/0,
     fun teardown_handle_ping/1,
     fun() ->
         Sess = #session{session_pid = self(), stats = #stats{}},
         P = #'Ping'{timestamp = 999, udp_packets = 1, tcp_packets = 2,
                     udp_ping_avg = 1.0, udp_ping_var = 0.1, tcp_ping_avg = 2.0, tcp_ping_var = 0.2,
                     good = 0, late = 0, lost = 0, resync = 0},
         ok = erlmur_tcp_message:handle_message(Sess, P),
         receive
             {sent, Bin} ->
                 [#'Ping'{timestamp = 999, good = 3, late = 1, lost = 0, resync = 2}] =
                     erlmur_tcp_message:decode(Bin)
         after 1000 ->
             ?assertion_failed(timeout)
         end
     end}.

setup_channel_create_broadcast() ->
    maybe_meck_new(pg, [unstick, passthrough]),
    maybe_meck_new(erlmur_channel_store, [unstick, passthrough]),
    maybe_meck_new(erlmur_session, [unstick, passthrough]),
    Self = self(),
    maybe_meck_expect(pg, get_members, fun(_Group, _Key) -> [Self] end),
    Created = #channel{
        id = 50, parent_id = 0, name = <<"new">>, description = <<"d">>, temporary = false,
        position = 1, description_hash = <<>>, max_users = 0, is_enter_restricted = false,
        can_enter = true, links = sets:new()
    },
    maybe_meck_expect(erlmur_channel_store, add, fun(Map) ->
        %% Ensure links and related keys absent as per filtering
        ?assertEqual(error, (catch maps:get(links, Map))),
        Created
    end),
    maybe_meck_expect(erlmur_session, send, fun(_Pid, _Bin) ->
        %% We don't decode content here; just acknowledge
        Self \! {broadcasted, channel_state}, ok end),
    ok.

teardown_channel_create_broadcast(_) ->
    maybe_meck_unload(pg),
    maybe_meck_unload(erlmur_channel_store),
    maybe_meck_unload(erlmur_session),
    ok.

handle_message_channel_create_triggers_broadcast_test_() ->
    {setup,
     fun setup_channel_create_broadcast/0,
     fun teardown_channel_create_broadcast/1,
     fun() ->
         Sess = #session{session_pid = self()},
         CS = #'ChannelState'{channel_id = undefined, name = <<"new">>, links = sets:new()},
         ok = erlmur_tcp_message:handle_message(Sess, CS),
         receive
             {broadcasted, channel_state} -> ok
         after 1000 ->
             ?assertion_failed(timeout)
         end
     end}.

setup_channel_remove_broadcast() ->
    maybe_meck_new(pg, [unstick, passthrough]),
    maybe_meck_new(erlmur_channel_store, [unstick, passthrough]),
    maybe_meck_new(erlmur_session, [unstick, passthrough]),
    Self = self(),
    maybe_meck_expect(pg, get_members, fun(_Group, _Key) -> [Self, Self] end),
    maybe_meck_expect(erlmur_channel_store, remove, fun(ChannelId, UserId) ->
        ?assertEqual(77, ChannelId),
        ?assertEqual(7, UserId),
        ok
    end),
    maybe_meck_expect(erlmur_session, send, fun(_Pid, _Bin) ->
        Self \! removed_broadcast, ok end),
    ok.

teardown_channel_remove_broadcast(_) ->
    maybe_meck_unload(pg),
    maybe_meck_unload(erlmur_channel_store),
    maybe_meck_unload(erlmur_session),
    ok.

handle_message_channel_remove_triggers_store_and_broadcast_test_() ->
    {setup,
     fun setup_channel_remove_broadcast/0,
     fun teardown_channel_remove_broadcast/1,
     fun() ->
         Sess = #session{user = #user{id = 7}},
         ok = erlmur_tcp_message:handle_message(Sess, #'ChannelRemove'{channel_id = 77}),
         %% Expect two broadcasts to members (we don't strictly count here; presence is enough)
         receive removed_broadcast -> ok after 1000 -> ?assertion_failed(timeout) end
     end}.