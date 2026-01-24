-module(mumble_varint).

-moduledoc """
Variable-length integer encoding and decoding.

This module implements the Mumble protocol's variable-length integer (varint)
encoding scheme. Varints are used throughout the protocol to efficiently
encode integers of varying sizes, using fewer bytes for smaller values.

## Encoding Scheme

The encoding uses a prefix to indicate the number of bytes:
- `0xxxxxxx` - 1 byte (7 bits value)
- `10xxxxxx` - 2 bytes (14 bits value)
- `110xxxxx` - 3 bytes (21 bits value)
- `1110xxxx` - 4 bytes (28 bits value)
- `11110000` - 5 bytes (32 bits value)
- `11110100` - 9 bytes (64 bits value)
- `111111xx` - 1 byte (2 bits value, negative numbers -4 to -1)
- `11111100` - 9 bytes (64 bits, negative numbers)

## Usage

```erlang
%% Encode a small integer (uses 1 byte)
<<0:1, 42:7>> = mumble_varint:encode(42).

%% Encode a larger integer (uses 2 bytes)
<<2#10:2, 1000:14>> = mumble_varint:encode(1000).

%% Decode
{42, <<>>} = mumble_varint:decode(<<0:1, 42:7>>).
```
""".

-export([encode/1, decode/1]).

-doc """
Encode an integer into variable-length binary format.
Input: Integer to encode (supports negative and positive values).
Output: Binary containing the variable-length encoded integer.
""".
-spec encode(integer()) -> binary().
encode(Num) when -4 < Num andalso Num < 0 ->
    <<2#111111:6, (-Num):2>>;
encode(Num) when Num < 0 ->
    <<2#11111100:8, (-(Num + 1)):64>>;
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

-doc """
Decode a variable-length binary format into an integer.
Input: Binary containing variable-length encoded integer.
Output: {integer(), binary()} tuple with decoded integer and remaining binary.
""".
-spec decode(binary()) -> {integer(), binary()}.
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
    {I, Rest};
decode(<<2#111111:6, I:2, Rest/binary>>) when I > 0 ->
    {-I, Rest};
decode(<<2#11111100:8, I:64, Rest/binary>>) ->
    {-(I + 1), Rest}.
