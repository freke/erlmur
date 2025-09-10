-module(erlmur_protocol_version).

-moduledoc "Handles the encoding and decoding of Mumble protocol version "
"numbers.\n\nThis module provides utility functions to convert "
"between the internal version\nrepresentation and the various "
"integer formats used in the Mumble protocol.".

-export([
    encode/1,
    decode/1,
    is_version_less_than/2,
    is_version_greater_than_or_equal_to/2
]).

%% defines #version{}
-include("erlmur.hrl").

-doc "Encode a #version{} record into both V1 and V2 integer formats.".

-spec encode(#version{}) -> {non_neg_integer(), non_neg_integer()}.
encode(Version) ->
    <<V1:32>> =
        <<(Version#version.major):16, (Version#version.minor):8, (Version#version.patch):8>>,
    <<V2:64>> =
        <<
            (Version#version.major):16, (Version#version.minor):16, (Version#version.patch):16, 0:16
        >>,
    {V1, V2}.

-doc "Decode a 32-bit or 64-bit encoded version integer into #version{}.".

-spec decode(non_neg_integer()) -> #version{}.
decode(V1) when V1 >= 0, V1 =< 16#FFFFFFFF ->
    <<Major:16, Minor:8, Patch:8>> = <<V1:32>>,
    #version{
        major = Major,
        minor = Minor,
        patch = Patch
    };
decode(V2) when V2 > 16#FFFFFFFF, V2 =< 16#FFFFFFFFFFFFFFFF ->
    <<Major:16, Minor:16, Patch:16, _Zero:16>> = <<V2:64>>,
    #version{
        major = Major,
        minor = Minor,
        patch = Patch
    }.

is_version_less_than(Version, {Major, Minor, Patch}) ->
    {Version#version.major, Version#version.minor, Version#version.patch} <
        {Major, Minor, Patch}.

is_version_greater_than_or_equal_to(Version, {Major, Minor, Patch}) ->
    {Version#version.major, Version#version.minor, Version#version.patch} >=
        {Major, Minor, Patch}.
