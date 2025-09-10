-module(erlmur_udp_message).

-export([handle/2]).

-include("MumbleUDP_gpb.hrl").
-include("erlmur.hrl").

handle(Session, <<1:3, Timestamp/bits>>) ->
    handle_ping(Session, Timestamp, false);
handle(Session, <<Type:3, Target:5, Rest/binary>>) ->
    logger:debug("DataMsg~nType ~p~nTarget ~p", [Type, Target]),
    {Counter, R} = erlmur_varint:decode(Rest),
    {Voice, Positional} = split_voice_positional(Type, R),
    gen_statem:cast(Session, {voice_data, Type, Target, Counter, Voice, Positional}).

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
    {OpusHeader, R0} = erlmur_varint:decode(Data),
    Len = OpusHeader band bnot 16#2000,
    <<V:Len/binary, R1/binary>> = R0,
    {<<(erlmur_varint:encode(OpusHeader))/binary, V/binary>>, R1}.

handle_ping(Session, Timestamp, _Extended) ->
    MumbleProtocol = Session#session.mumble_protocol,
    Pong =
        case MumbleProtocol of
            v1_5 ->
                <<T:64>> = Timestamp,
                PongMsg = #'Ping'{timestamp = T},
                PongBin = 'MumbleUDP_gpb':encode_msg(PongMsg),
                <<1:3, 0:5, PongBin/binary>>;
            _ ->
                <<1:3, Timestamp/bits>>
        end,
    logger:debug("Reply ping ~p ~p", [MumbleProtocol, binary:encode_hex(Pong)]),
    erlmur_session:send_udp(Session#session.session_pid, Pong).
