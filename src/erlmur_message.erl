%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013,
%%% @doc
%%%
%%% @end
%%% Created : 17 Mar 2013 by  <davabe@hotmail.com>

-module(erlmur_message).

-export(
	[
		pack/1,
		data_msg/1,
		control_msg/1,
		unpack/2,
		version/1,
		userremove/1,
		userstate/1,
		channelstate/1,
		channelremove/1,
		proplist/1
	]).

-include("mumble_gpb.hrl").
-include_lib("record_info/include/record_info.hrl").

-export_record_info(
	[
		'Authenticate',
		'Ping',
		'CryptSetup',
		'ServerSync',
		'Version',
		'UserState',
		'UserRemove',
		'UserList',
		'UserStats',
		'BanList',
		'CodecVersion',
		'ServerConfig',
		'PermissionQuery',
		'ChannelState',
		'ChannelRemove',
    'TextMessage'
	]).

%% 0
-define(MSG_VERSION, 16#00).
-define(MSG_UDPTUNNEL, 16#01).
-define(MSG_AUTHENTICATE, 16#02).
-define(MSG_PING, 16#03).
-define(MSG_REJECT, 16#04).
-define(MSG_SERVERSYNC, 16#05).
-define(MSG_CHANNELREMOVE, 16#06).
-define(MSG_CHANNELSTATE, 16#07).
-define(MSG_USERREMOVE, 16#08).
-define(MSG_USERSTATE, 16#09).
%% 10
-define(MSG_BANLIST, 16#0A).
-define(MSG_TEXTMESSAGE, 16#0B).
-define(MSG_PERMISSONDENIED, 16#0C).
-define(MSG_ACL, 16#0D).
-define(MSG_QUERYUSERS, 16#0E).
-define(MSG_CRYPTSETUP, 16#0F).
-define(MSG_CONTEXTACTIONADD, 16#10).
-define(MSG_CONTEXTACTION, 16#11).
-define(MSG_USERLIST, 16#12).
-define(MSG_VOICETARGET, 16#13).
%% 20
-define(MSG_PERMISSIONQUERY, 16#14).
-define(MSG_CODECVERSION, 16#15).
-define(MSG_USERSTATS, 16#16).
-define(MSG_REQUESTBLOB, 16#17).
-define(MSG_SERVERCONFIG, 16#18).
-define(MSG_SUGGESTCONFIG, 16#19).

pack({authenticate,PropList}) ->
    A = authenticate(PropList),
    R = mumble_gpb:encode_msg(A),
    encode_message(?MSG_AUTHENTICATE,R);
pack({ping,PropList}) ->
    V = ping(PropList),
    R = mumble_gpb:encode_msg(V),
    encode_message(?MSG_PING,R);
pack({version,PropList}) ->
    V = version(PropList),
    R = mumble_gpb:encode_msg(V),
    encode_message(?MSG_VERSION,R);
pack({userstate,PropList}) ->
    US = userstate(PropList),
    R = mumble_gpb:encode_msg(US),
    encode_message(?MSG_USERSTATE,R);
pack({userstats,PropList}) ->
    US = userstats(PropList),
    R = mumble_gpb:encode_msg(US),
    encode_message(?MSG_USERSTATS,R);
pack({channelstate,PropList}) ->
    CS = channelstate(PropList),
    R = mumble_gpb:encode_msg(CS),
    encode_message(?MSG_CHANNELSTATE,R);
pack({channelremove,PropList}) ->
    CR = channelremove(PropList),
    R = mumble_gpb:encode_msg(CR),
    encode_message(?MSG_CHANNELREMOVE,R);
pack({userremove,PropList}) ->
    UR = userremove(PropList),
    R = mumble_gpb:encode_msg(UR),
    encode_message(?MSG_USERREMOVE,R);
pack({userlist,Users}) ->
    U = lists:map(fun({Id,Name}) -> #'UserList.User'{user_id=Id,name=Name} end, Users),
    R = mumble_gpb:encode_msg(#'UserList'{users=U}),
    encode_message(?MSG_USERLIST,R);
pack({banlist,Users}) ->
    U = lists:map(fun({Id,Name}) -> #'UserList.User'{user_id=Id,name=Name} end, Users),
    R = mumble_gpb:encode_msg(#'BanList'{bans=U}),
    encode_message(?MSG_BANLIST,R);
pack({cryptsetup,PropList}) ->
    CS = cryptsetup(PropList),
    R = mumble_gpb:encode_msg(CS),
    encode_message(?MSG_CRYPTSETUP,R);
pack({codecversion,PropList}) ->
    CV = codecversion(PropList),
    R = mumble_gpb:encode_msg(CV),
    encode_message(?MSG_CODECVERSION,R);
pack({serverconfig,PropList}) ->
    SC = serverconfig(PropList),
    R = mumble_gpb:encode_msg(SC),
    encode_message(?MSG_SERVERCONFIG,R);
pack({serversync,PropList}) ->
    SS = serversync(PropList),
    R = mumble_gpb:encode_msg(SS),
    encode_message(?MSG_SERVERSYNC,R);
pack({permissionquery,PropList}) ->
    PQ = permissionquery(PropList),
    R = mumble_gpb:encode_msg(PQ),
    encode_message(?MSG_PERMISSIONQUERY,R);
pack({udp_tunnel,Data}) ->
    encode_message(?MSG_UDPTUNNEL,Data);
pack({textmessage,PropList}) ->
    TM = textmessage(PropList),
    R = mumble_gpb:encode_msg(TM),
    encode_message(?MSG_TEXTMESSAGE,R).

data_msg(<<1:3,0:5,_Timestamp/binary>> = PingMsg) ->
    PingMsg;
data_msg(<<Type:3,Target:5,Rest/binary>>) ->
    {Counter,R} = erlmur_varint:decode(Rest),
    {Voice,Positional} = split_voice_positional(Type,R),
    {voice_data,Type,Target,Counter,Voice,Positional}.

control_msg(<<>>) ->
    [];
control_msg(<<Type:16/unsigned-big-integer, Len:32/unsigned-big-integer, Msg:Len/binary, Rest/binary>>) ->
    [{Type,Msg}|control_msg(Rest)].

unpack(?MSG_VERSION,Msg) ->
    {version, proplist(mumble_gpb:decode_msg(Msg, 'Version'))};
unpack(?MSG_AUTHENTICATE,Msg) ->
    {authenticate, proplist(mumble_gpb:decode_msg(Msg, 'Authenticate'))};
unpack(?MSG_PERMISSIONQUERY,Msg) ->
    {permissionquery, proplist(mumble_gpb:decode_msg(Msg, 'PermissionQuery'))};
unpack(?MSG_PING,Msg) ->
    {ping, proplist(mumble_gpb:decode_msg(Msg, 'Ping'))};
unpack(?MSG_CRYPTSETUP,Msg) ->
    {cryptsetup, proplist(mumble_gpb:decode_msg(Msg, 'CryptSetup'))};
unpack(?MSG_UDPTUNNEL, Msg) ->
    {udptunnel, Msg};
unpack(?MSG_CHANNELSTATE, Msg) ->
    {channelstate, proplist(mumble_gpb:decode_msg(Msg, 'ChannelState'))};
unpack(?MSG_CHANNELREMOVE, Msg) ->
    {channelremove, proplist(mumble_gpb:decode_msg(Msg, 'ChannelRemove'))};
unpack(?MSG_USERSTATE, Msg) ->
    {userstate, proplist(mumble_gpb:decode_msg(Msg, 'UserState'))};
unpack(?MSG_USERREMOVE, Msg) ->
    {userremove, proplist(mumble_gpb:decode_msg(Msg, 'UserRemove'))};
unpack(?MSG_USERLIST, Msg) ->
    {userlist, proplist(mumble_gpb:decode_msg(Msg, 'UserList'))};
unpack(?MSG_BANLIST, Msg) ->
    {banlist, proplist(mumble_gpb:decode_msg(Msg, 'BanList'))};
unpack(?MSG_USERSTATS, Msg) ->
    {userstats, proplist(mumble_gpb:decode_msg(Msg, 'UserStats'))};
unpack(?MSG_TEXTMESSAGE, Msg) ->
    {textmessage, proplist(mumble_gpb:decode_msg(Msg, 'TextMessage'))};
unpack(?MSG_CODECVERSION, Msg) ->
    {codecversion, proplist(mumble_gpb:decode_msg(Msg, 'CodecVersion'))};
unpack(?MSG_SERVERCONFIG, Msg) ->
    {serverconfig, proplist(mumble_gpb:decode_msg(Msg, 'ServerConfig'))};
unpack(?MSG_SERVERSYNC, Msg) ->
    {serversync, proplist(mumble_gpb:decode_msg(Msg, 'ServerSync'))}.



proplist(Record) ->
    record_info:record_to_proplist(Record, ?MODULE).

authenticate(PropList) ->
    record_info:proplist_to_record(PropList, 'Authenticate', ?MODULE).

ping(PropList) ->
    record_info:proplist_to_record(PropList, 'Ping', ?MODULE).

version(PropList) ->
    record_info:proplist_to_record(PropList, 'Version', ?MODULE).

userstate(PropList) ->
    record_info:proplist_to_record(PropList, 'UserState', ?MODULE).

userstats(PropList) ->
    record_info:proplist_to_record(PropList, 'UserStats', ?MODULE).

userremove(PropList) ->
    record_info:proplist_to_record(PropList, 'UserRemove', ?MODULE).

channelstate(PropList) ->
    record_info:proplist_to_record(PropList, 'ChannelState', ?MODULE).

channelremove(PropList) ->
    record_info:proplist_to_record(PropList, 'ChannelRemove', ?MODULE).

cryptsetup(PropList) ->
    record_info:proplist_to_record(PropList, 'CryptSetup', ?MODULE).

codecversion(PropList) ->
    record_info:proplist_to_record(PropList, 'CodecVersion', ?MODULE).

serverconfig(PropList) ->
    record_info:proplist_to_record(PropList, 'ServerConfig', ?MODULE).

serversync(PropList) ->
    record_info:proplist_to_record(PropList, 'ServerSync', ?MODULE).

permissionquery(PropList) ->
    record_info:proplist_to_record(PropList, 'PermissionQuery', ?MODULE).

textmessage(PropList) ->
    record_info:proplist_to_record(PropList, 'TextMessage', ?MODULE).

encode_message(Type, Msg) when is_list(Msg) ->
    encode_message(Type, erlang:iolist_to_binary(Msg));
encode_message(Type, Msg) when is_binary(Msg) ->
    Len = byte_size(Msg),
    <<Type:16/unsigned-big-integer, Len:32/unsigned-big-integer,Msg/binary>>.

split_voice_positional(4,Data) ->
    {Data,<<>>};
split_voice_positional(_,Data) ->
    split_voice_positional(Data).

split_voice_positional(<<1:1,Len:7,V1:Len/binary,Rest/binary>>) ->
    {V2,R1} = split_voice_positional(Rest),
    {<<1:1,Len:7,V1:Len/binary,V2/binary>>,R1};
split_voice_positional(<<0:1,Len:7,V:Len/binary,Rest/binary>>) ->
    {<<0:1,Len:7,V:Len/binary>>,Rest}.
