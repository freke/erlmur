-module(mumble_version).

-moduledoc """
Handles the encoding and decoding of Mumble protocol version numbers.

This module provides utility functions to convert between the internal version
representation and the various integer formats used in the Mumble protocol.
""".

-export([encode/1, decode/1, is_version_less_than/2,
         is_version_greater_than_or_equal_to/2]).

-include("mumble_protocol.hrl").

-doc """
Encode a #version{} record into both V1 and V2 integer formats.
Input: #version{} record with major, minor, patch fields.
Output: {V1_32bit, V2_64bit} tuple of encoded version integers.
""".
-spec encode(#version{}) -> {non_neg_integer(), non_neg_integer()}.
encode(Version) ->
    <<V1:32>> =
        <<(Version#version.major):16, (Version#version.minor):8, (Version#version.patch):8>>,
    <<V2:64>> =
        <<(Version#version.major):16,
          (Version#version.minor):16,
          (Version#version.patch):16,
          0:16>>,
    {V1, V2}.

-doc """
Decode a 32-bit or 64-bit encoded version integer into #version{}.
Input: Encoded version integer (V1 32-bit or V2 64-bit format).
Output: #version{} record with major, minor, patch fields.
""".
-spec decode(non_neg_integer()) -> #version{}.
decode(V1) when V1 >= 0, V1 =< 16#FFFFFFFF ->
    <<Major:16, Minor:8, Patch:8>> = <<V1:32>>,
    #version{major = Major,
             minor = Minor,
             patch = Patch};
decode(V2) when V2 > 16#FFFFFFFF, V2 =< 16#FFFFFFFFFFFFFFFF ->
    <<Major:16, Minor:16, Patch:16, _Zero:16>> = <<V2:64>>,
    #version{major = Major,
             minor = Minor,
             patch = Patch}.

-doc """
Check if a version is less than a given major.minor.patch tuple.
Input: #version{} record and {Major, Minor, Patch} tuple.
Output: true if version is less, false otherwise.
""".
-spec is_version_less_than(#version{}, {non_neg_integer(), non_neg_integer(), non_neg_integer()}) -> boolean().
is_version_less_than(Version, {Major, Minor, Patch}) ->
    {Version#version.major, Version#version.minor, Version#version.patch}
    < {Major, Minor, Patch}.

-doc """
Check if a version is greater than or equal to a given major.minor.patch tuple.
Input: #version{} record and {Major, Minor, Patch} tuple.
Output: true if version is greater or equal, false otherwise.
""".
-spec is_version_greater_than_or_equal_to(#version{}, {non_neg_integer(), non_neg_integer(), non_neg_integer()}) -> boolean().
is_version_greater_than_or_equal_to(Version, {Major, Minor, Patch}) ->
    {Version#version.major, Version#version.minor, Version#version.patch}
    >= {Major, Minor, Patch}.
