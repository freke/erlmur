-module(mumble_client_api_tests).

-include_lib("eunit/include/eunit.hrl").
-include("mumble_protocol.hrl").

%% Test the mumble client API using meck mocking

mumble_client_api_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"client start with no certs (connection fails)", fun client_start_no_certs/0},
         {"client start with invalid certs", fun client_start_invalid_certs/0},
         {"client connect and send message", fun client_send_message/0},
         {"client connect and send voice", fun client_send_voice/0},
         {"client connect and get state", fun client_get_state/0},
         {"client connect and stop", fun client_stop/0},
         {"client invalid ref handling", fun client_invalid_ref/0}
     ]}.

setup() ->
    %% Unload any existing mocks first
    catch meck:unload(ssl),
    catch meck:unload(mumble_client_conn),
    catch meck:unload(mumble_cert),
    ok.

cleanup(_) ->
    %% Clean up all mocks
    catch meck:unload(ssl),
    catch meck:unload(mumble_client_conn),
    catch meck:unload(mumble_cert),
    %% Clean up ETS table
    catch ets:delete(mock_client_pids),
    ok.

%% Mock helper: Set up SSL mocks for successful connection
mock_ssl_success() ->
    meck:new(ssl, [unstick, passthrough]),
    MockSocket = spawn(fun mock_ssl_socket/0),
    meck:expect(ssl, connect, fun(_Host, _Port, _Opts) -> {ok, MockSocket} end),
    meck:expect(ssl, setopts, fun(_Socket, _Opts) -> ok end),
    meck:expect(ssl, send, fun(_Socket, _Data) -> ok end),
    meck:expect(ssl, close, fun(_Socket) -> ok end),
    meck:expect(ssl, controlling_process, fun(_Socket, _Pid) -> ok end),
    MockSocket.

%% Mock SSL socket process that simulates server responses
mock_ssl_socket() ->
    mock_ssl_socket_loop(#{}).

mock_ssl_socket_loop(State) ->
    receive
        {send, Data, From} ->
            %% Store the sent data for verification
            NewState = State#{last_sent => Data},
            From ! {ssl_send_ack, self()},
            mock_ssl_socket_loop(NewState);
        {get_state, From} ->
            From ! {ssl_state, State},
            mock_ssl_socket_loop(State);
        {inject, Data} ->
            %% Simulate receiving data from server
            case State of
                #{owner := Owner} when Owner =/= undefined ->
                    Owner ! {ssl, self(), Data};
                _ ->
                    ok
            end,
            mock_ssl_socket_loop(State);
        {setopts, Opts} ->
            NewState = case lists:keyfind(active, 1, Opts) of
                {active, _} -> State#{active => true};
                _ -> State
            end,
            mock_ssl_socket_loop(NewState);
        {controlling_process, Pid} ->
            mock_ssl_socket_loop(State#{owner => Pid});
        stop ->
            ok
    after 5000 ->
        mock_ssl_socket_loop(State)
    end.

%% Mock helper: Set up mumble_client_conn mocks
mock_client_conn() ->
    meck:new(mumble_client_conn, [unstick]),
    
    %% Create ETS table to track stopped processes
    ets:new(mock_client_pids, [public, named_table]),
    
    %% Mock start_link to return a fake PID
    meck:expect(mumble_client_conn, start_link, fun(_Host, _Port, _Opts) ->
        Pid = spawn(fun fake_client_conn/0),
        ets:insert(mock_client_pids, {Pid, active}),
        {ok, Pid}
    end),
    
    %% Mock send
    meck:expect(mumble_client_conn, send, fun(Pid, _Msg) ->
        case ets:lookup(mock_client_pids, Pid) of
            [{Pid, active}] -> ok;
            _ -> {error, invalid_client_ref}
        end
    end),
    
    %% Mock send_voice
    meck:expect(mumble_client_conn, send_voice, fun(Pid, _Msg) ->
        case ets:lookup(mock_client_pids, Pid) of
            [{Pid, active}] -> ok;
            _ -> {error, invalid_client_ref}
        end
    end),
    
    %% Mock get_state
    meck:expect(mumble_client_conn, get_state, fun(Pid) ->
        case ets:lookup(mock_client_pids, Pid) of
            [{Pid, active}] -> #{session_id => 1, connected => true};
            _ -> {error, invalid_client_ref}
        end
    end),
    
    %% Mock stop
    meck:expect(mumble_client_conn, stop, fun(Pid) ->
        case ets:lookup(mock_client_pids, Pid) of
            [{Pid, active}] ->
                ets:delete(mock_client_pids, Pid),
                Pid ! stop,
                ok;
            _ ->
                {error, invalid_client_ref}
        end
    end).

%% Fake client connection process
fake_client_conn() ->
    receive
        stop -> ok;
        _ -> fake_client_conn()
    after 30000 -> ok
    end.

client_start_no_certs() ->
    mock_client_conn(),
    
    %% Test starting client without certificates
    {ok, ClientRef} = mumble:start_client(undefined, undefined, "localhost", 64738),
    ?assertMatch({mumble_client, _}, ClientRef),
    
    ok = mumble:stop_client(ClientRef),
    ?assert(meck:validate(mumble_client_conn)).

client_start_invalid_certs() ->
    %% Test starting client with invalid certificate files
    ?assertEqual({error, {cert_file_not_found, "/nonexistent/cert.pem"}}, 
                 mumble:start_client("/nonexistent/cert.pem", "/nonexistent/key.pem", "localhost", 64738)).

client_send_message() ->
    mock_client_conn(),
    
    {ok, ClientRef} = mumble:start_client(undefined, undefined, "localhost", 64738),
    
    %% Send a ping message
    TestMsg = #{message_type => 'Ping', timestamp => 12345},
    ?assertEqual(ok, mumble:send(ClientRef, TestMsg)),
    
    %% Verify send was called on mumble_client_conn
    ?assert(meck:called(mumble_client_conn, send, '_')),
    
    ok = mumble:stop_client(ClientRef),
    ?assert(meck:validate(mumble_client_conn)).

client_send_voice() ->
    mock_client_conn(),
    
    {ok, ClientRef} = mumble:start_client(undefined, undefined, "localhost", 64738),
    
    %% Send voice data
    VoiceMsg = {voice_data, 0, 0, 1, <<1, 2, 3, 4>>, undefined},
    ?assertEqual(ok, mumble:send_voice(ClientRef, VoiceMsg)),
    
    %% Verify send_voice was called
    ?assert(meck:called(mumble_client_conn, send_voice, '_')),
    
    ok = mumble:stop_client(ClientRef),
    ?assert(meck:validate(mumble_client_conn)).

client_get_state() ->
    mock_client_conn(),
    
    {ok, ClientRef} = mumble:start_client(undefined, undefined, "localhost", 64738),
    
    {ok, State} = mumble:get_state(ClientRef),
    ?assert(is_map(State)),
    
    ok = mumble:stop_client(ClientRef),
    ?assert(meck:validate(mumble_client_conn)).

client_stop() ->
    mock_client_conn(),
    
    {ok, ClientRef} = mumble:start_client(undefined, undefined, "localhost", 64738),
    
    %% Stop should succeed
    ?assertEqual(ok, mumble:stop_client(ClientRef)),
    
    %% Stopping again should fail
    ?assertEqual({error, invalid_client_ref}, mumble:stop_client(ClientRef)),
    
    ?assert(meck:validate(mumble_client_conn)).

client_invalid_ref() ->
    %% Test operations with invalid client references
    InvalidRef = {invalid_ref, self()},
    
    ?assertEqual({error, invalid_client_ref}, mumble:send(InvalidRef, #{message_type => 'Ping'})),
    ?assertEqual({error, invalid_client_ref}, mumble:send_voice(InvalidRef, {voice_data, 0, 0, 1, <<>>, undefined})),
    ?assertEqual({error, invalid_client_ref}, mumble:get_state(InvalidRef)),
    ?assertEqual({error, invalid_client_ref}, mumble:stop_client(InvalidRef)).
