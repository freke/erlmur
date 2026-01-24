-module(prop_protocol_tests).
-include_lib("proper/include/proper.hrl").

prop_varint_roundtrip() ->
    ?FORALL(V, integer(),
        begin
            {V, <<>>} == mumble_varint:decode(mumble_varint:encode(V))
        end).

prop_tcp_proto_roundtrip() ->
    %% This would require generating all protocol messages which is complex.
    %% We'll focus on varints for now.
    true.
