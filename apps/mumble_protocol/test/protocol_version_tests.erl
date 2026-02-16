-module(protocol_version_tests).
-include_lib("eunit/include/eunit.hrl").
-include("mumble_protocol.hrl").

encode_decode_test() ->
    V = #version{major = 1, minor = 2, patch = 4},
    {V1, V2} = mumble_version:encode(V),
    ?assertEqual(V, mumble_version:decode(V1)),
    ?assertEqual(V, mumble_version:decode(V2)).

comparison_test() ->
    V = #version{major = 1, minor = 2, patch = 4},
    ?assert(mumble_version:is_version_less_than(V, {1, 2, 5})),
    ?assert(mumble_version:is_version_less_than(V, {1, 3, 0})),
    ?assert(mumble_version:is_version_less_than(V, {2, 0, 0})),
    ?assertNot(mumble_version:is_version_less_than(V, {1, 2, 4})),
    
    ?assert(mumble_version:is_version_greater_than_or_equal_to(V, {1, 2, 4})),
    ?assert(mumble_version:is_version_greater_than_or_equal_to(V, {1, 2, 3})),
    ?assertNot(mumble_version:is_version_greater_than_or_equal_to(V, {1, 2, 5})).
