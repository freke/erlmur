-module(mumble_udp_proto).

-moduledoc """
UDP protocol message handler for Mumble voice traffic.

This module handles incoming UDP packets from Mumble clients, parsing the
protocol-specific binary format and routing to appropriate handlers.

## UDP Protocol Format

UDP packets use a compact binary format:
- **Type (3 bits)**: Message type (0=Ping, 1=Voice, etc.)
- **Target (5 bits)**: Target user/channel for voice packets
- **Counter**: Varint-encoded packet sequence number
- **Payload**: Voice data or ping payload

## Supported Message Types

- Type 1: Ping (used for UDP liveness detection)
- Type 0, 2-7: Voice packets with various codecs
""".

-export([handle/2]).

-include("MumbleUDP_gpb.hrl").
-include("mumble_protocol.hrl").

-doc """
Handle incoming UDP protocol message from a client.
Input: Session record and binary packet data.
Output: Routes packet to appropriate handler (ping or voice data).
""".
-spec handle(#session{}, binary()) -> ok.
handle(Session, <<1:3, Timestamp/bits>>) ->
    handle_ping(Session, Timestamp, false);
handle(Session, <<Type:3, Target:5, Rest/binary>>) ->
    logger:debug("DataMsg~nType ~p~nTarget ~p", [Type, Target]),
    {Counter, R} = mumble_varint:decode(Rest),
    {Voice, Positional} = split_voice_positional(Type, R),
    mumble_server_conn:voice_data(Session#session.session_pid,
                              {voice_data, Type, Target, Counter, Voice, Positional}).

%%%%%%
%%% Private
%%%%%%

split_voice_positional(4, Data) ->
    split_voice_positional_opus(Data);
split_voice_positional(_, Data) ->
    split_voice_positional_speex_celt(Data).

split_voice_positional_speex_celt(<<1:1, Len:7, V1:Len/binary, Rest/binary>>) ->
    {V2, R1} = split_voice_positional_speex_celt(Rest),
    {<<1:1, Len:7, V1:Len/binary, V2/binary>>, R1};
split_voice_positional_speex_celt(<<0:1, Len:7, V:Len/binary, Rest/binary>>) ->
    {<<0:1, Len:7, V:Len/binary>>, Rest}.

split_voice_positional_opus(Data) ->
    {OpusHeader, R0} = mumble_varint:decode(Data),
    Len = OpusHeader band bnot 16#2000,
    <<V:Len/binary, R1/binary>> = R0,
    {<<(mumble_varint:encode(OpusHeader))/binary, V/binary>>, R1}.

handle_ping(Session, Timestamp, _Extended) ->
    MumbleProtocol = Session#session.mumble_protocol,
    Pong =
        case MumbleProtocol of
            v1_5 ->
                <<T:64>> = Timestamp,
                PongMap = #{message_type => 'Ping', timestamp => T},
                PongRecord = mumble_msg:from_map(PongMap),
                PongBin = 'MumbleUDP_gpb':encode_msg(PongRecord),
                <<1:3, 0:5, PongBin/binary>>;
            _ ->
                <<1:3, Timestamp/bits>>
        end,
    logger:debug("Reply ping ~p ~p", [MumbleProtocol, binary:encode_hex(Pong)]),
    mumble_server_conn:send_udp(Session#session.session_pid, Pong).
