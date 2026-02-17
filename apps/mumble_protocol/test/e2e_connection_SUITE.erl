-module(e2e_connection_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("Mumble_gpb.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
         end_per_testcase/2]).
-export([basic_connection_test/1, ping_test/1, text_echo_test/1, voice_fallback_test/1, udp_switch_test/1]).

all() ->
    [basic_connection_test, ping_test, text_echo_test, voice_fallback_test, udp_switch_test].

init_per_suite(Config) ->
		application:load(erlmur),
    PrivDir = ?config(priv_dir, Config),
    CertFile = filename:join(PrivDir, "cert.pem"),
    KeyFile = filename:join(PrivDir, "key.pem"),
    os:cmd("openssl req -x509 -newkey rsa:2048 -keyout "
           ++ KeyFile
           ++ " -out "
           ++ CertFile
           ++ " -days 1 -nodes -subj '/CN=localhost'"),
		ct:log("Key and Cert file ~p ~p",[KeyFile, CertFile]),

    application:set_env(erlmur, listen_port, 0),
    application:set_env(erlmur, cert_pem, CertFile),
    application:set_env(erlmur, key_pem, KeyFile),
    application:set_env(erlmur, allow_selfsigned, true),

    {ok, _} = application:ensure_all_started(mnesia),
    {ok, _} = application:ensure_all_started(erlmur),

    [{cert_pem, CertFile}, {key_pem, KeyFile} | Config].

end_per_suite(_Config) ->
    application:stop(erlmur),
    application:stop(mnesia),
    ok.

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

basic_connection_test(Config) ->
    {MockMumblePort, CertFile, KeyFile, Name} = setup_mock_server(Config, basic_connection),
    Opts = [{certfile, CertFile}, {keyfile, KeyFile}],
    {ok, ClientPid} = mumble_client_conn:start_link({127, 0, 0, 1}, MockMumblePort, Opts),

    ok = wait_for_established(ClientPid, 5000),

    gen_statem:stop(ClientPid),
    ranch:stop_listener(Name),
    ok.

ping_test(Config) ->
    {MockMumblePort, CertFile, KeyFile, Name} = setup_mock_server(Config, ping_test),
    Opts = [{certfile, CertFile}, {keyfile, KeyFile}],
    {ok, ClientPid} = mumble_client_conn:start_link({127, 0, 0, 1}, MockMumblePort, Opts),
    ok = wait_for_established(ClientPid, 5000),

    Ping = #{message_type => 'Ping', timestamp => 42, good => 100},
    mumble_client_conn:send(ClientPid, Ping),

    ok = wait_for_ping(42),

    gen_statem:stop(ClientPid),
    ranch:stop_listener(Name),
    ok.

wait_for_ping(Timestamp) ->
    receive
        {mumble_msg, #{message_type := 'Ping', timestamp := TS} = M} when TS == Timestamp ->
            ct:pal("Received expected Ping: ~p", [M]),
            %% Verify we don't echo back the statistics we sent (100)
            verify_ping_response(M),
            ok;
        {mumble_msg, #{message_type := Type}} when 
            Type == 'Version'; Type == 'CryptSetup'; 
            Type == 'CodecVersion'; Type == 'ServerSync' ->
            %% Skip handshake msgs
            wait_for_ping(Timestamp);
        {mumble_msg, M} ->
            ct:pal("Received unexpected msg while waiting for ping: ~p", [M]),
            wait_for_ping(Timestamp)
    after 2000 ->
        ct:fail(ping_timeout)
    end.

verify_ping_response(#{good := _}) -> ct:fail(good_should_be_undefined);
verify_ping_response(_M) -> ok.

text_echo_test(Config) ->
    {MockMumblePort, CertFile, KeyFile, Name} = setup_mock_server(Config, text_echo_test),
    Opts = [{certfile, CertFile}, {keyfile, KeyFile}],
    {ok, ClientPid} = mumble_client_conn:start_link({127, 0, 0, 1}, MockMumblePort, Opts),
    ok = wait_for_established(ClientPid, 5000),

    Text = #{message_type => 'TextMessage', message => <<"Hello">>},
    mumble_client_conn:send(ClientPid, Text),

    ok = wait_for_text(<<"Echo: Hello">>),

    gen_statem:stop(ClientPid),
    ranch:stop_listener(Name),
    ok.

wait_for_text(Expected) ->
    receive
        {mumble_msg, #{message_type := 'TextMessage', message := Msg} = M} when Msg == Expected ->
            ct:pal("Received expected TextMessage: ~p", [M]),
            ok;
        {mumble_msg, #{message_type := Type}} when 
            Type == 'Version'; Type == 'CryptSetup'; 
            Type == 'CodecVersion'; Type == 'ServerSync' ->
            %% Skip handshake msgs
            wait_for_text(Expected);
        {mumble_msg, M} ->
            ct:pal("Received unexpected msg while waiting for text: ~p", [M]),
            wait_for_text(Expected)
    after 2000 ->
        ct:fail(echo_timeout)
    end.

voice_fallback_test(Config) ->
    {MockMumblePort, CertFile, KeyFile, Name} = setup_mock_server(Config, voice_fallback_test),
    Opts = [{certfile, CertFile}, {keyfile, KeyFile}],
    {ok, ClientPid} = mumble_client_conn:start_link({127, 0, 0, 1}, MockMumblePort, Opts),
    ok = wait_for_established(ClientPid, 5000),

    %% Packet Type 0 (CELT Alpha), Target 31 (Server Loopback), Seq 0
    %% Payload must match format expected by splitting logic.
    %% <<0:1, Len:7, Data:Len/bytes>>
    %% Len=5, Data="VOICE". <<5, "VOICE">>
    VoicePayload = <<5, "VOICE">>,
    VoicePacket = {voice_data, 0, 31, 0, VoicePayload, undefined},
    
    %% UDP is not verified yet, so Client should send via TCP Tunnel (Msg Type 1)
    mumble_client_conn:send_voice(ClientPid, VoicePacket),

    %% Server receives via TCP, sees Target=31 (loopback), echoes back via TCP
    ok = wait_for_udp_tunnel_voice(VoicePayload, 1000),

    gen_statem:stop(ClientPid),
    ranch:stop_listener(Name),
    ok.

udp_switch_test(Config) ->
    {MockMumblePort, CertFile, KeyFile, Name} = setup_mock_server(Config, udp_switch_test),
    Opts = [{certfile, CertFile}, {keyfile, KeyFile}],
    {ok, ClientPid} = mumble_client_conn:start_link({127, 0, 0, 1}, MockMumblePort, Opts),
    ok = wait_for_established(ClientPid, 5000),

    %% Simulate Client receiving a UDP packet (which verifies UDP on client side)
    ClientPid ! {udp, undefined, {127,0,0,1}, MockMumblePort, <<"DummyPacket">>},

    %% Give it a moment to process state change
    timer:sleep(100),

    {StateName, StateData} = sys:get_state(ClientPid),
    established = StateName,
    %% Accessing the record field via element/2 since record def is not exported
    %% -record(state, {socket, transport = ssl, session_id, parent, stats = #stats{}, udp_verified = false, udp_timer}).
    %% 1:state, 2:socket, 3:transport, 4:session_id, 5:parent, 6:stats, 7:udp_verified, 8:udp_timer
    true = element(7, StateData),

    gen_statem:stop(ClientPid),
    ranch:stop_listener(Name),
    ok.

wait_for_udp_tunnel_voice(ExpectedData, Timeout) ->
    receive
        {mumble_msg, #{message_type := 'UDPTunnel', packet := Packet}} ->
            %% Server packet format: Type(3) | Context(5) | SenderSession(varint) | Counter(varint) | Data
            <<0:3, 0:5, Rest/bits>> = Packet,
            {_SenderSession, Rest2} = mumble_varint:decode(Rest),
            {_Seq, VoiceData} = mumble_varint:decode(Rest2),
            case VoiceData of
                ExpectedData -> ok;
                _ -> 
                    ct:pal("Received wrong voice data: ~p", [VoiceData]),
                    wait_for_udp_tunnel_voice(ExpectedData, Timeout)
            end;
        {mumble_msg, #{message_type := Type}} when
            Type == 'Version'; Type == 'CryptSetup'; Type == 'CodecVersion'; 
            Type == 'ServerSync'; Type == 'Ping'; 
            Type == 'UserState'; Type == 'ChannelState' ->
            %% Ignore other messages
            wait_for_udp_tunnel_voice(ExpectedData, Timeout);
        {mumble_msg, Other} ->
             ct:pal("Received unexpected: ~p", [Other]),
             wait_for_udp_tunnel_voice(ExpectedData, Timeout)
    after Timeout ->
        %% ct:fail(timeout_waiting_for_voice)
        {error, timeout}
    end.

setup_mock_server(Config, Name) ->
    CertFile = ?config(cert_pem, Config),
    KeyFile = ?config(key_pem, Config),
    SslOpts = [{certfile, CertFile}, {keyfile, KeyFile}],
    ListenPort = 0,
    StartArgs = [mock_mumble_handler, []],
    {ok, _} = ranch:start_listener(Name, ranch_ssl, [{port, ListenPort} | SslOpts], mumble_server_conn, StartArgs),
    MockMumblePort = ranch:get_port(Name),
    {MockMumblePort, CertFile, KeyFile, Name}.

wait_for_established(Pid, Timeout) when Timeout > 0 ->
    {StateName, _Data} = sys:get_state(Pid),
    case StateName of
        established ->
            ok;
        _ ->
            timer:sleep(100),
            wait_for_established(Pid, Timeout - 100)
    end;
wait_for_established(_, _) ->
    ct:fail(timeout).
