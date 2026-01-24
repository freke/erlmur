-module(mumble_tcp_proto).

-moduledoc """
Handles the packing and unpacking of Mumble protocol messages.

This module serializes Erlang records into binary format for network transmission
and deserializes incoming binary data into Erlang records.
""".

-export([decode/1, pack/1]).

-include("Mumble_gpb.hrl").
-include("mumble_protocol.hrl").

%% 0
-define(MSG_VERSION, 16#00).
-define(MSG_UDPTUNNEL, 16#01).
-define(MSG_AUTHENTICATE, 16#02).
-define(MSG_PING, 16#03).
% -define(MSG_REJECT, 16#04).
-define(MSG_SERVERSYNC, 16#05).
-define(MSG_CHANNELREMOVE, 16#06).
-define(MSG_CHANNELSTATE, 16#07).
-define(MSG_USERREMOVE, 16#08).
-define(MSG_USERSTATE, 16#09).
%% 10
-define(MSG_BANLIST, 16#0A).
-define(MSG_TEXTMESSAGE, 16#0B).
% -define(MSG_PERMISSONDENIED, 16#0C).
% -define(MSG_ACL, 16#0D).
% -define(MSG_QUERYUSERS, 16#0E).
-define(MSG_CRYPTSETUP, 16#0F).
% -define(MSG_CONTEXTACTIONADD, 16#10).
% -define(MSG_CONTEXTACTION, 16#11).
-define(MSG_USERLIST, 16#12).
% -define(MSG_VOICETARGET, 16#13).
%% 20
-define(MSG_PERMISSIONQUERY, 16#14).
-define(MSG_CODECVERSION, 16#15).
-define(MSG_USERSTATS, 16#16).
% -define(MSG_REQUESTBLOB, 16#17).
-define(MSG_SERVERCONFIG, 16#18).
% -define(MSG_SUGGESTCONFIG, 16#19).
-define(MESSAGE_TABLE,
        [{?MSG_VERSION, #'Version'{}},
         {?MSG_UDPTUNNEL, #'UDPTunnel'{}},
         {?MSG_AUTHENTICATE, #'Authenticate'{}},
         {?MSG_PING, #'Ping'{}},
         {?MSG_SERVERSYNC, #'ServerSync'{}},
         {?MSG_USERSTATE, #'UserState'{}},
         {?MSG_USERREMOVE, #'UserRemove'{}},
         {?MSG_USERLIST, #'UserList'{}},
         {?MSG_USERSTATS, #'UserStats'{}},
         {?MSG_BANLIST, #'BanList'{}},
         {?MSG_TEXTMESSAGE, #'TextMessage'{}},
         {?MSG_CRYPTSETUP, #'CryptSetup'{}},
         {?MSG_CHANNELSTATE, #'ChannelState'{}},
         {?MSG_CHANNELREMOVE, #'ChannelRemove'{}},
         {?MSG_CODECVERSION, #'CodecVersion'{}},
         {?MSG_SERVERCONFIG, #'ServerConfig'{}},
         {?MSG_PERMISSIONQUERY, #'PermissionQuery'{}}]).

-doc """
Decodes a binary payload from a TCP message into a list of Mumble message records.

The function processes the binary data and returns a list of decoded records.
If the payload is incomplete or contains an unknown message type, it logs
an error and attempts to decode the rest of the payload.
""".
-spec decode(binary()) -> [any()].
decode(Data) ->
    unpack(Data, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-doc """
Pack a message map into a binary frame for transmission.

Converts a message map (with `message_type` field) into the binary format
used by the Mumble TCP protocol. The binary format includes:
- 2 bytes: Message type tag
- 4 bytes: Payload length
- N bytes: Serialized message

Input: `MessageMap` - Map containing `message_type` and message fields.
Output: Binary encoded for TCP transmission.

### Example
```erlang
Bin = mumble_tcp_proto:pack(#{
    message_type => 'Ping',
    timestamp => 12345
}).
```
""".
-spec pack(map()) -> binary().
pack(MessageMap) when is_map(MessageMap) ->
    logger:debug("pack ~p", [MessageMap]),
    Record = mumble_msg:from_map(MessageMap),
    case find_msg_by_record(Record) of
        {ok, Tag, _} ->
            Bin = 'Mumble_gpb':encode_msg(Record),
            encode_message(Tag, Bin);
        error ->
            error({unknown_type, Record})
    end.

unpack(<<>>, Acc) ->
    lists:reverse(Acc);
unpack(<<Type:16/unsigned-big-integer,
         Len:32/unsigned-big-integer,
         Msg:Len/binary,
         Rest/binary>>,
       Acc) ->
    case find_msg_by_tag(Type) of
        {ok, _Type, Record} ->
            DecodedRecord = 'Mumble_gpb':decode_msg(Msg, element(1, Record)),
            logger:debug("unpack ~p", [element(1, DecodedRecord)]),
            DecodedMap = mumble_msg:to_map(DecodedRecord),
            unpack(Rest, [DecodedMap | Acc]);
        error ->
            logger:error("Unable to unpack Msg ~p~nLen ~p~nMsg ~p", [Type, Len, Msg]),
            unpack(Rest, Acc)
    end.

find_msg_by_record(Record) ->
    RecordName = element(1, Record),
    case lists:search(fun({_Tag, R}) -> is_record(R, RecordName) end, ?MESSAGE_TABLE) of
        {value, {Tag, _}} ->
            {ok, Tag, Record};
        false ->
            error
    end.

find_msg_by_tag(Tag) ->
    lists:foldl(fun ({T, Record}, _Acc) when T =:= Tag ->
                        {ok, Tag, Record};
                    (_, Acc) ->
                        Acc
                end,
                error,
                ?MESSAGE_TABLE).

encode_message(Type, Msg) when is_binary(Msg) ->
    Len = byte_size(Msg),
    <<Type:16/unsigned-big-integer, Len:32/unsigned-big-integer, Msg/binary>>.
