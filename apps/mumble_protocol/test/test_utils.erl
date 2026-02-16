-module(test_utils).

-moduledoc """
Common testing utilities for Mumble protocol tests.

This module provides helper functions for creating test data, managing
mock objects, and asserting test conditions across all test modules.
""".

-export([create_version_message/0, create_authenticate_message/1, create_ping_message/0]).
-export([wait_for_message/2, wait_for_messages/2, assert_received/2]).
-export([encode_message/1, decode_messages/1]).
-export([start_mock_handler/0, stop_mock_handler/1]).

-include("mumble_protocol.hrl").
-include("Mumble_gpb.hrl").

-doc """
Create a Version message for testing.
Returns a map representing the Version message.
""".
-spec create_version_message() -> map().
create_version_message() ->
    #{
        message_type => 'Version',
        version_v1 => 66052,  %% 1.2.4
        version_v2 => 282578800168960,  %% Encoded version
        release => <<"TestClient">>,
        os => <<"TestOS">>,
        os_version => <<"1.0">>
    }.

-doc """
Create an Authenticate message for testing.
Input: Username string or binary.
Returns a map representing the Authenticate message.
""".
-spec create_authenticate_message(string() | binary()) -> map().
create_authenticate_message(Username) when is_list(Username) ->
    create_authenticate_message(list_to_binary(Username));
create_authenticate_message(Username) when is_binary(Username) ->
    #{
        message_type => 'Authenticate',
        username => Username,
        password => <<"">>,
        tokens => [],
        celt_versions => [],
        opus => true
    }.

-doc """
Create a Ping message for testing.
Returns a map representing the Ping message.
""".
-spec create_ping_message() -> map().
create_ping_message() ->
    #{
        message_type => 'Ping',
        timestamp => erlang:system_time(millisecond)
    }.

-doc """
Wait for a specific message to be received.
Input: Pattern to match against and timeout in milliseconds.
Returns: {ok, Message} if received, timeout if not.
""".
-spec wait_for_message(term(), non_neg_integer()) -> {ok, term()} | timeout.
wait_for_message(Pattern, Timeout) ->
    receive
        Pattern = Msg ->
            {ok, Msg}
    after Timeout ->
        timeout
    end.

-doc """
Wait for multiple messages matching a pattern.
Input: Pattern to match against and timeout in milliseconds.
Returns: List of matched messages.
""".
-spec wait_for_messages(term(), non_neg_integer()) -> list().
wait_for_messages(Pattern, Timeout) ->
    wait_for_messages(Pattern, Timeout, []).

wait_for_messages(_Pattern, 0, Acc) ->
    lists:reverse(Acc);
wait_for_messages(Pattern, Timeout, Acc) ->
    Start = erlang:monotonic_time(millisecond),
    receive
        Pattern = Msg ->
            Elapsed = erlang:monotonic_time(millisecond) - Start,
            wait_for_messages(Pattern, Timeout - Elapsed, [Msg | Acc])
    after Timeout ->
        lists:reverse(Acc)
    end.

-doc """
Assert that a specific message was received.
Input: Pattern to match and timeout.
Raises an error if message not received within timeout.
""".
-spec assert_received(term(), non_neg_integer()) -> ok | no_return().
assert_received(Pattern, Timeout) ->
    case wait_for_message(Pattern, Timeout) of
        {ok, _} ->
            ok;
        timeout ->
            error({assertion_failed, expected_message, Pattern})
    end.

-doc """
Encode a message map to binary format.
Input: Message map.
Returns: Binary encoded message.
""".
-spec encode_message(map()) -> binary().
encode_message(Msg) ->
    mumble_tcp_proto:pack(Msg).

-doc """
Decode binary data into message maps.
Input: Binary data.
Returns: List of message maps.
""".
-spec decode_messages(binary()) -> list(map()).
decode_messages(Data) ->
    mumble_tcp_proto:decode(Data).

-doc """
Start a mock handler module for testing.
Returns: {ok, HandlerPid} where HandlerPid implements mumble_server_behaviour.
""".
-spec start_mock_handler() -> {ok, pid()}.
start_mock_handler() ->
    mock_mumble_handler:start_link([]).

-doc """
Stop a mock handler.
Input: Handler PID.
""".
-spec stop_mock_handler(pid()) -> ok.
stop_mock_handler(Pid) ->
    mock_mumble_handler:stop(Pid).
