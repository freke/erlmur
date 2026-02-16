-module(server_logic_tests).

-moduledoc """
Comprehensive unit tests for mumble_server_conn state machine.
""".

-include_lib("eunit/include/eunit.hrl").
-include("mumble_protocol.hrl").
-include("Mumble_gpb.hrl").
-include_lib("ocb128_crypto/include/ocb128_crypto.hrl").



-define(TEST_TIMEOUT, 5000).

init_test_() ->
    {"Server connection initialization", fun() ->
        mock_transport:setup_ranch_mock(),
        try
            {ok, Pid} = mumble_server_conn:start_link(make_ref(), mock_transport, [mock_mumble_handler]),
            timer:sleep(100),
            ?assert(is_pid(Pid)),
            ?assertEqual(true, is_process_alive(Pid)),
            catch gen_statem:stop(Pid)
        after
            mock_transport:cleanup_ranch_mock()
        end,
        ok
    end}.

init_with_handler_test_() ->
    {"Server connection with handler", fun() ->
        mock_transport:setup_ranch_mock(),
        try
            {ok, Pid} = mumble_server_conn:start_link(make_ref(), mock_transport, [mock_mumble_handler, [{user, "TestUser"}]]),
            timer:sleep(100),
            State = mumble_server_conn:get_state(Pid),
            ?assert(is_tuple(State)),
            catch gen_statem:stop(Pid)
        after
            mock_transport:cleanup_ranch_mock()
        end,
        ok
    end}.

get_state_test_() ->
    {"Get state API", fun() ->
        mock_transport:setup_ranch_mock(),
        try
            {ok, Pid} = mumble_server_conn:start_link(make_ref(), mock_transport, [mock_mumble_handler]),
            timer:sleep(50),
            
            State = mumble_server_conn:get_state(Pid),
            ?assert(is_tuple(State)),
            
            catch gen_statem:stop(Pid)
        after
            mock_transport:cleanup_ranch_mock()
        end,
        ok
    end}.

send_message_test_() ->
    {"Send message API", fun() ->
        mock_transport:setup_ranch_mock(),
        try
            {ok, Pid} = mumble_server_conn:start_link(make_ref(), mock_transport, [mock_mumble_handler]),
            
            Msg = #{
                message_type => 'Ping',
                timestamp => 12345
            },
            Result = mumble_server_conn:send(Pid, Msg),
            ?assertEqual(ok, Result),
            
            catch gen_statem:stop(Pid)
        after
            mock_transport:cleanup_ranch_mock()
        end,
        ok
    end}.

voice_data_test_() ->
    {"Voice data handling", fun() ->
        mock_transport:setup_ranch_mock(),
        try
            {ok, Pid} = mumble_server_conn:start_link(make_ref(), mock_transport, [mock_mumble_handler]),
            timer:sleep(50),
            
            VoiceMsg = {voice_data, 0, 0, 1, <<"voice_data">>, undefined},
            Result = mumble_server_conn:voice_data(Pid, VoiceMsg),
            ?assertEqual(ok, Result),
            
            catch gen_statem:stop(Pid)
        after
            mock_transport:cleanup_ranch_mock()
        end,
        ok
    end}.

invalid_transition_test_() ->
    {"Invalid state transitions", fun() ->
        mock_transport:setup_ranch_mock(),
        try
            {ok, Pid} = mumble_server_conn:start_link(make_ref(), mock_transport, [mock_mumble_handler]),
            
            VoiceMsg = {voice_data, 0, 0, 1, <<"voice">>, undefined},
            mumble_server_conn:voice_data(Pid, VoiceMsg),
            
            timer:sleep(50),
            
            ?assertEqual(true, is_process_alive(Pid)),
            
            catch gen_statem:stop(Pid)
        after
            mock_transport:cleanup_ranch_mock()
        end,
        ok
    end}.
