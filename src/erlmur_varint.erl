-module(erlmur_varint).

-export([encode/1, decode/1]).

encode(Num) when -4 < Num andalso Num < 0 ->
    <<16#FC:6, -Num:2>>;
encode(Num) when Num < 0 ->
    <<16#FC:8, -Num:64>>;
encode(Num) when Num < 16#80 ->
    <<2#0:1, Num:7>>;
encode(Num) when Num < 16#4000 ->
    <<2#10:2, Num:14>>;
encode(Num) when Num < 16#200000 ->
    <<2#110:3, Num:21>>;
encode(Num) when Num < 16#10000000 ->
    <<2#1110:4, Num:28>>;
encode(Num) when Num < 16#100000000 ->
    <<2#11110000:8, Num:32>>;
encode(Num) ->
    <<2#11110100:8, Num:64>>.

decode(<<0:1, I:7, Rest/binary>>) ->
    {I, Rest};
decode(<<1:1, 0:1, I:14, Rest/binary>>) ->
    {I, Rest};
decode(<<2#11:2, 0:1, I:21, Rest/binary>>) ->
    {I, Rest};
decode(<<2#111:3, 0:1, I:28, Rest/binary>>) ->
    {I, Rest};
decode(<<2#1111:4, 0:2, _:2, I:32, Rest/binary>>) ->
    {I, Rest};
decode(<<2#1111:4, 0:1, 1:1, _:2, I:64, Rest/binary>>) ->
    {I, Rest}.
