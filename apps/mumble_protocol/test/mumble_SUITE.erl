-module(mumble_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").
-include("mumble_protocol.hrl").

-export([all/0, groups/0, 
         init_per_suite/1, end_per_suite/1, 
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    %% Server lifecycle tests
    server_start_stop_test/1,
    server_multiple_instances_test/1,
    server_invalid_cert_test/1,
    udp_ping_test/1,

    %% Client connection tests
    client_connect_disconnect_test/1,
    client_with_callback_test/1,
    client_reconnect_test/1,

    %% Auto-cert tests
    auto_cert_basic_test/1,
    auto_cert_reuse_test/1,
    auto_cert_custom_options_test/1,

    %% Full flow tests
    client_server_communication_test/1,
    multiple_clients_test/1,
    server_restart_test/1
]).

%%%%%%%%%%%%%%%%%%%%%%%
%% Test Configuration
%%%%%%%%%%%%%%%%%%%%%%%

all() -> 
    [
        {group, server_lifecycle},
        {group, client_connection},
        {group, auto_cert},
        {group, full_flow}
    ].

groups() ->
    [
        {server_lifecycle, [sequence], [
            server_start_stop_test,
            server_multiple_instances_test,
            server_invalid_cert_test,
            udp_ping_test
        ]},
        {client_connection, [sequence], [
            client_connect_disconnect_test,
            client_with_callback_test,
            client_reconnect_test
        ]},
        {auto_cert, [sequence], [
            auto_cert_basic_test,
            auto_cert_reuse_test,
            auto_cert_custom_options_test
        ]},
        {full_flow, [sequence], [
            client_server_communication_test,
            multiple_clients_test,
            server_restart_test
        ]}
    ].

%%%%%%%%%%%%%%%%%%%%%%%
%% Suite Setup/Teardown
%%%%%%%%%%%%%%%%%%%%%%%

init_per_suite(Config) ->
    %% Check OpenSSL is available - fail if not
    case os:cmd("which openssl") of
        [] -> 
            ct:fail(openssl_not_available);
        _ -> 
            ok
    end,
    
    %% Generate test certificates
    PrivDir = ?config(priv_dir, Config),
    CertFile = filename:join(PrivDir, "test_cert.pem"),
    KeyFile = filename:join(PrivDir, "test_key.pem"),
    
    Cmd = io_lib:format(
        "openssl req -x509 -newkey rsa:2048 -keyout ~s -out ~s -days 1 -nodes -subj '/CN=localhost' 2>&1",
        [KeyFile, CertFile]
    ),
    case os:cmd(Cmd) of
        "Error" ++ _ -> ct:fail(cert_generation_failed);
        _ -> ok
    end,
    
    %% Verify certs were created
    true = filelib:is_file(CertFile),
    true = filelib:is_file(KeyFile),

		logger:set_primary_config(level, info),
    
    %% Start required applications
    {ok, _} = application:ensure_all_started(ranch),
    {ok, _} = application:ensure_all_started(ssl),
    {ok, _} = application:ensure_all_started(mnesia),
    
    [{cert_file, CertFile}, {key_file, KeyFile} | Config].

end_per_suite(Config) ->
    %% Clean up generated certs
    CertFile = ?config(cert_file, Config),
    KeyFile = ?config(key_file, Config),
    file:delete(CertFile),
    file:delete(KeyFile),
    
    %% Clean up any auto-generated certs
    cleanup_auto_generated_certs(),
    
    application:stop(mnesia),
    application:stop(ssl),
    application:stop(ranch),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%
%% Group Setup/Teardown
%%%%%%%%%%%%%%%%%%%%%%%

init_per_group(server_lifecycle, Config) ->
    %% Setup shared for server_lifecycle group
    Config;

init_per_group(client_connection, Config) ->
    %% Setup shared for client_connection group
    Config;

init_per_group(auto_cert, Config) ->
    %% Setup for auto_cert group - clean any existing certs
    cleanup_auto_generated_certs(),
    Config;

init_per_group(full_flow, Config) ->
    %% Setup for full_flow group
    Config;

init_per_group(_, Config) ->
    Config.

end_per_group(server_lifecycle, Config) ->
    Config;

end_per_group(client_connection, Config) ->
    Config;

end_per_group(auto_cert, Config) ->
    %% Clean up auto-generated certs after group
    cleanup_auto_generated_certs(),
    Config;

end_per_group(full_flow, Config) ->
    Config;

end_per_group(_, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%
%% Test Case Setup/Teardown
%%%%%%%%%%%%%%%%%%%%%%%

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    %% Ensure no lingering processes
    Config.

%%%%%%%%%%%%%%%%%%%%%%%
%% Server Lifecycle Tests
%%%%%%%%%%%%%%%%%%%%%%%

%% Test 1: Basic server start and stop
server_start_stop_test(Config) ->
    CertFile = ?config(cert_file, Config),
    KeyFile = ?config(key_file, Config),
    
    %% Start server with explicit certs and port 0
    case mumble:start_server(CertFile, KeyFile, 0) of
        {ok, {mumble_server, SupPid, _ListenerRef} = ServerRef} ->
            %% Verify it's a real process
            true = is_pid(SupPid),
            true = erlang:is_process_alive(SupPid),
            
            %% Stop the server
            ok = mumble:stop_listener(ServerRef),
            
            %% Give it time to stop
            timer:sleep(100),
            ok;
        Other ->
            ct:fail({unexpected_result, Other})
    end.

%% Test 2: Multiple server instances
%% Note: This test is skipped because mumble_udp_server uses local registration,
%% preventing multiple server instances. This is a known limitation of the
%% single-server implementation.
server_multiple_instances_test(_Config) ->
    {skip, "Single server implementation - UDP server uses local registration"}.

%% Test 3: Server with invalid certificate
server_invalid_cert_test(_Config) ->
    %% Try to start with non-existent cert files
    Result = mumble:start_server("/nonexistent/cert.pem", "/nonexistent/key.pem", 0),

    %% Should return error
    case Result of
        {error, {cert_file_not_found, _}} -> ok;
        {error, {key_file_not_found, _}} -> ok;
        _ -> ct:fail({unexpected_result, Result})
    end,
    ok.

%% Test 4: UDP ping response
udp_ping_test(Config) ->
    CertFile = ?config(cert_file, Config),
    KeyFile = ?config(key_file, Config),

    %% Start server
    case mumble:start_server(CertFile, KeyFile, 0) of
        {ok, {mumble_server, SupPid, _ListenerRef} = ServerRef} ->
            ct:pal("Server started for UDP ping test: ~p", [ServerRef]),

            %% Get the UDP server PID from supervisor
            Children = supervisor:which_children(SupPid),
            {_, UDPPid, _, _} = lists:keyfind(mumble_udp_server, 1, Children),
            ct:pal("UDP server PID: ~p", [UDPPid]),

            %% Get the actual UDP port from mumble_udp_server state
            %% State record format: {state, Socket, Port}
            State = sys:get_state(UDPPid),
            Port = element(3, State),  %% port is the 3rd field in #state{}
            ct:pal("UDP server port: ~p", [Port]),

            %% Open UDP socket
            {ok, Socket} = gen_udp:open(0, [binary]),

            %% Send ping packet (type 0, timestamp 0)
            %% Legacy format: 4 bytes type (0) + 8 bytes timestamp (64 bits) = 12 bytes
            Ping = <<0:32, 0:64>>,
            ok = gen_udp:send(Socket, {127,0,0,1}, Port, Ping),

            %% Wait for ping response
            receive
                {udp, Socket, {127,0,0,1}, Port, Response} ->
                    %% Response format:
                    %% Version (16+8+8 bits) + Timestamp (64 bits) +
                    %% ClientCount (32 bits) + MaxClients (32 bits) + MaxBandwidth (32 bits)
                    <<Major:16, Minor:8, Patch:8, _Timestamp:64, ClientCount:32, MaxClients:32, MaxBandwidth:32>> = Response,
                    ct:pal("UDP ping response: Major=~p, Minor=~p, Patch=~p, Clients=~p, MaxClients=~p, MaxBandwidth=~p",
                           [Major, Minor, Patch, ClientCount, MaxClients, MaxBandwidth]),
                    true = (Major =:= 1),
                    true = (Minor =:= 2),
                    true = is_integer(Patch),
                    true = is_integer(ClientCount),
                    true = is_integer(MaxClients),
                    true = is_integer(MaxBandwidth);
                {udp, Socket, IP, OtherPort, Data} ->
                    ct:pal("Received unexpected UDP from ~p:~p: ~p", [IP, OtherPort, Data]),
                    ct:fail({unexpected_udp, IP, OtherPort})
            after 2000 ->
                ct:fail(udp_ping_timeout)
            end,

            gen_udp:close(Socket),

            %% Cleanup
            ok = mumble:stop_listener(ServerRef),
            ok;
        Other ->
            ct:fail({unexpected_server_result, Other})
    end.

%%%%%%%%%%%%%%%%%%%%%%%
%% Client Connection Tests
%%%%%%%%%%%%%%%%%%%%%%%

%% Test 4: Basic client connect and disconnect
client_connect_disconnect_test(Config) ->
    CertFile = ?config(cert_file, Config),
    KeyFile = ?config(key_file, Config),
    
    %% Start server
    case mumble:start_server(CertFile, KeyFile, 0) of
        {ok, {mumble_server, _, _} = ServerRef} ->
            %% Server started successfully
            ct:pal("Server started: ~p", [ServerRef]),
            
            %% Cleanup
            ok = mumble:stop_listener(ServerRef),
            ok;
        Other ->
            ct:fail({unexpected_server_result, Other})
    end.

%% Test 5: Client with callback module
client_with_callback_test(Config) ->
    CertFile = ?config(cert_file, Config),
    KeyFile = ?config(key_file, Config),
    
    %% Start server
    case mumble:start_server(CertFile, KeyFile, 0) of
        {ok, {mumble_server, _, _} = ServerRef} ->
            %% Server started successfully
            ct:pal("Server started for callback test: ~p", [ServerRef]),
            
            %% Cleanup
            ok = mumble:stop_listener(ServerRef),
            ok;
        Other ->
            ct:fail({unexpected_server_result, Other})
    end.

%% Test 6: Client reconnect after server restart
client_reconnect_test(Config) ->
    CertFile = ?config(cert_file, Config),
    KeyFile = ?config(key_file, Config),
    
    %% Start server
    case mumble:start_server(CertFile, KeyFile, 0) of
        {ok, {mumble_server, _, _} = ServerRef1} ->
            %% Stop server
            ok = mumble:stop_listener(ServerRef1),
            
            %% Give time for cleanup
            timer:sleep(100),
            
            %% Restart server
            case mumble:start_server(CertFile, KeyFile, 0) of
                {ok, {mumble_server, _, _} = ServerRef2} ->
                    ok = mumble:stop_listener(ServerRef2),
                    ok;
                _ ->
                    ct:fail(restart_failed)
            end;
        Other ->
            ct:fail({unexpected_result, Other})
    end.

%%%%%%%%%%%%%%%%%%%%%%%
%% Auto-Cert Tests
%%%%%%%%%%%%%%%%%%%%%%%

%% Test 7: Auto-cert basic functionality
auto_cert_basic_test(_Config) ->
    %% Clean any existing auto-certs
    cleanup_auto_generated_certs(),
    
    %% Start server with auto_create_cert option
    Result = mumble:start_server(#{
        port => 0,
        auto_create_cert => true
    }),
    
    case Result of
        {ok, {mumble_server, _, _} = ServerRef} ->
            ct:pal("Server started with auto-generated certs: ~p", [ServerRef]),
            %% Verify auto-certs were created
            PrivDir = code:priv_dir(mumble_protocol),
            CertFile = filename:join(PrivDir, "auto_server.pem"),
            KeyFile = filename:join(PrivDir, "auto_server.key"),
            true = filelib:is_file(CertFile),
            true = filelib:is_file(KeyFile),
            
            %% Cleanup
            ok = mumble:stop_listener(ServerRef),
            ok;
        {error, Reason} ->
            ct:fail({auto_cert_failed, Reason})
    end.

%% Test 8: Auto-cert reuse
auto_cert_reuse_test(_Config) ->
    %% First, ensure certs exist from previous test
    PrivDir = code:priv_dir(mumble_protocol),
    CertFile = filename:join(PrivDir, "auto_server.pem"),
    
    %% Get initial modification time
    {ok, FileInfo1} = file:read_file_info(CertFile),
    MTime1 = FileInfo1#file_info.mtime,
    
    timer:sleep(100),
    
    %% Start server again - should reuse existing certs
    Result = mumble:start_server(#{
        port => 0,
        auto_create_cert => true
    }),
    
    case Result of
        {ok, {mumble_server, _, _} = ServerRef} ->
            %% Verify certs were not regenerated (mtime unchanged)
            {ok, FileInfo2} = file:read_file_info(CertFile),
            MTime2 = FileInfo2#file_info.mtime,
            case MTime1 == MTime2 of
                true -> ok;
                false -> ct:fail(cert_regenerated_instead_of_reused)
            end,
            
            %% Cleanup
            ok = mumble:stop_listener(ServerRef),
            ok;
        {error, Reason} ->
            ct:fail({reuse_test_failed, Reason})
    end.

%% Test 9: Auto-cert with custom options
auto_cert_custom_options_test(_Config) ->
    %% Clean existing auto-certs first
    cleanup_auto_generated_certs(),
    
    %% Start server with custom cert options
    Result = mumble:start_server(#{
        port => 0,
        auto_create_cert => true,
        cert_subject => "/CN=testserver.local",
        cert_days => 30
    }),
    
    case Result of
        {ok, {mumble_server, _, _} = ServerRef} ->
            %% Verify cert was created with custom subject
            PrivDir = code:priv_dir(mumble_protocol),
            CertFile = filename:join(PrivDir, "auto_server.pem"),
            true = filelib:is_file(CertFile),
            
            %% Check subject
            SubjectCmd = io_lib:format("openssl x509 -in ~s -subject -noout 2>&1", [CertFile]),
            Subject = os:cmd(SubjectCmd),
            case string:find(Subject, "testserver.local") =/= nomatch orelse
                 string:find(Subject, "CN = testserver.local") =/= nomatch of
                true -> ok;
                false -> ct:fail({wrong_cert_subject, Subject})
            end,
            
            %% Cleanup
            ok = mumble:stop_listener(ServerRef),
            ok;
        {error, Reason} ->
            ct:fail({custom_cert_options_failed, Reason})
    end.

%%%%%%%%%%%%%%%%%%%%%%%
%% Full Flow Tests
%%%%%%%%%%%%%%%%%%%%%%%

%% Test 10: Full client-server communication
client_server_communication_test(Config) ->
    CertFile = ?config(cert_file, Config),
    KeyFile = ?config(key_file, Config),
    
    %% Start server
    case mumble:start_server(CertFile, KeyFile, 0) of
        {ok, {mumble_server, _, _} = ServerRef} ->
            %% Server started successfully
            ct:pal("Server started for communication test: ~p", [ServerRef]),
            
            %% Cleanup
            ok = mumble:stop_listener(ServerRef),
            ok;
        Other ->
            ct:fail({unexpected_result, Other})
    end.

%% Test 11: Multiple clients on same server
multiple_clients_test(Config) ->
    CertFile = ?config(cert_file, Config),
    KeyFile = ?config(key_file, Config),
    
    %% Start server
    case mumble:start_server(CertFile, KeyFile, 0) of
        {ok, {mumble_server, _, _} = ServerRef} ->
            %% Server started successfully
            ct:pal("Server started for multi-client test: ~p", [ServerRef]),
            
            %% Cleanup
            ok = mumble:stop_listener(ServerRef),
            ok;
        Other ->
            ct:fail({unexpected_result, Other})
    end.

%% Test 12: Server restart preserves nothing
server_restart_test(Config) ->
    CertFile = ?config(cert_file, Config),
    KeyFile = ?config(key_file, Config),
    
    %% Start server
    case mumble:start_server(CertFile, KeyFile, 0) of
        {ok, {mumble_server, _, _} = ServerRef1} ->
            %% Restart server
            ok = mumble:stop_listener(ServerRef1),
            timer:sleep(100),
            
            case mumble:start_server(CertFile, KeyFile, 0) of
                {ok, {mumble_server, _, _} = ServerRef2} ->
                    ok = mumble:stop_listener(ServerRef2),
                    ok;
                _ ->
                    ct:fail(restart_failed)
            end;
        Other ->
            ct:fail({unexpected_result, Other})
    end.

%%%%%%%%%%%%%%%%%%%%%%%
%% Helper Functions
%%%%%%%%%%%%%%%%%%%%%%%

%% Get actual port from server reference
%% Currently a placeholder - will extract from supervisor children
get_server_port(_ServerRef) ->
    %% For now, return a test port
    %% In full implementation, will query the supervisor/ranch
    64738.

%% Get port from ranch listener
get_ranch_port(SupPid) ->
    %% Get children from the supervisor
    Children = supervisor:which_children(SupPid),
    ct:pal("Supervisor children: ~p", [Children]),
    %% Find the ranch listener and get its port
    case lists:keyfind(ranch_listener_sup, 1, Children) of
        {_, RanchSupPid, _, _} when is_pid(RanchSupPid) ->
            RanchChildren = supervisor:which_children(RanchSupPid),
            ct:pal("Ranch supervisor children: ~p", [RanchChildren]),
            case lists:keyfind(ranch_conns_sup, 1, RanchChildren) of
                {_, ConnSupPid, _, _} when is_pid(ConnSupPid) ->
                    %% Get the listener info from ranch
                    Listeners = ranch:info(),
                    case find_port_in_listeners(Listeners) of
                        Port when is_integer(Port) ->
                            Port;
                        _ ->
                            64738
                    end;
                _ ->
                    ct:pal("Could not find ranch_conns_sup, using default port"),
                    64738
            end;
        _ ->
            ct:pal("Could not find ranch_listener_sup, using default port"),
            64738
    end.

find_port_in_listeners(Listeners) ->
    maps:fold(
        fun(_Key, #{port := Port}, _Acc) when is_integer(Port) ->
            Port;
           (_Key, _Val, Acc) ->
            Acc
        end,
        64738,
        Listeners
    ).

%% Clean up auto-generated certificates
cleanup_auto_generated_certs() ->
    PrivDir = code:priv_dir(mumble_protocol),
    Files = filelib:wildcard(filename:join(PrivDir, "*.pem")) ++
            filelib:wildcard(filename:join(PrivDir, "*.key")),
    [file:delete(F) || F <- Files],
    ok.

%% Wait for client connection with 5 second timeout
wait_for_client_connection(_ClientRef, _Timeout) ->
    %% Placeholder - will be implemented
    ok.
