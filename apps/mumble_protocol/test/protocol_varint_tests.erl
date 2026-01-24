-module(protocol_varint_tests).
-include_lib("eunit/include/eunit.hrl").

varint_roundtrip_test_() ->
    Values = [0, 1, 127, 128, 16383, 16384, 2097151, 2097152, 268435455, 268435456, 4294967295, 4294967296],
    [?_assertEqual({V, <<>>}, mumble_varint:decode(mumble_varint:encode(V))) || V <- Values].

negative_varint_test_() ->
    Values = [-1, -2, -3, -4, -5, -100, -1000],
    %% Current implementation of decode seems to missing negative support, 
    %% this test might fail initially and help us identify the fix.
    [?_assertEqual({V, <<>>}, mumble_varint:decode(mumble_varint:encode(V))) || V <- Values].
