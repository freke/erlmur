%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 17 Mar 2013 by  <davabe@hotmail.com>

-module(erlmur_message).

-export([handle/2,handle_udp/2]).

-export([pack/1,data_msg/1,control_msg/1,unpack/2,version/1,userremove/1,userstate/1,channelstate/1,channelremove/1,proplist/1]).

-include("mumble_pb.hrl").
-include_lib("record_info/include/record_info.hrl").

-export_record_info([authenticate,
		     ping,
		     cryptsetup,
		     serversync,
		     version,
		     userstate,
		     userremove,
		     codecversion,
		     serverconfig,
		     channelstate,
		     permissionquery,
		     channelstate,
		     channelremove]).

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
-define(MSG_PERMISSIONQUERY, 16#14).
-define(MSG_CODECVERSION, 16#15).
-define(MSG_USERSTATA, 16#16).
-define(MSG_REQUESTBLOB, 16#17).
-define(MSG_SERVERCONFIG, 16#18).
-define(MSG_SUGGESTCONFIG, 16#19).

pack({authenticate,PropList}) ->
    A = authenticate(PropList),
    R=mumble_pb:encode_authenticate(A),
    encode_message(?MSG_AUTHENTICATE,R);
pack({ping,PropList}) ->
    V = ping(PropList),
    R=mumble_pb:encode_ping(V),
    encode_message(?MSG_PING,R);
pack({version,PropList}) ->
    V = version(PropList),
    R=mumble_pb:encode_version(V),
    encode_message(?MSG_VERSION,R);
pack({userstate,PropList}) ->
    US = userstate(PropList),
    R=mumble_pb:encode_userstate(US),
    encode_message(?MSG_USERSTATE,R);
pack({channelstate,PropList}) ->
    CS = channelstate(PropList),
    R=mumble_pb:encode_channelstate(CS),
    encode_message(?MSG_CHANNELSTATE,R);
pack({channelremove,PropList}) ->
    CR = channelremove(PropList),
    R=mumble_pb:encode_channelremove(CR),
    encode_message(?MSG_CHANNELREMOVE,R);
pack({userremove,PropList}) ->
    UR = userremove(PropList),
    R=mumble_pb:encode_userremove(UR),
    encode_message(?MSG_USERREMOVE,R);
pack({cryptsetup,PropList}) ->
    CS = cryptsetup(PropList),
    R=mumble_pb:encode_cryptsetup(CS),
    encode_message(?MSG_CRYPTSETUP,R);
pack({codecversion,PropList}) ->
    CV = codecversion(PropList),
    R=mumble_pb:encode_codecversion(CV),
    encode_message(?MSG_CODECVERSION,R);
pack({serverconfig,PropList}) ->
    SC = serverconfig(PropList),
    R=mumble_pb:encode_serverconfig(SC),
    encode_message(?MSG_SERVERCONFIG,R);
pack({serversync,PropList}) ->
    SS = serversync(PropList),
    R=mumble_pb:encode_serversync(SS),
    encode_message(?MSG_SERVERSYNC,R);
pack({permissionquery,PropList}) ->
    PQ = permissionquery(PropList),
    R=mumble_pb:encode_permissionquery(PQ),
    encode_message(?MSG_PERMISSIONQUERY,R);
pack({udp_tunnel,{_,Data}}) ->
    encode_message(?MSG_UDPTUNNEL,Data).
    

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
    {version,proplist(mumble_pb:decode_version(Msg))};
unpack(?MSG_AUTHENTICATE,Msg) ->
    {authenticate,proplist(mumble_pb:decode_authenticate(Msg))};
unpack(?MSG_PERMISSIONQUERY,Msg) ->
    {permissionquery,proplist(mumble_pb:decode_permissionquery(Msg))};
unpack(?MSG_PING,Msg) ->
    {ping,proplist(mumble_pb:decode_ping(Msg))};
unpack(?MSG_CRYPTSETUP,Msg) ->
    {cryptsetup,proplist(mumble_pb:decode_cryptsetup(Msg))};
unpack(?MSG_UDPTUNNEL, Msg) ->
    {udptunnel,Msg};
unpack(?MSG_CHANNELSTATE, Msg) ->
    {channelstate,proplist(mumble_pb:decode_channelstate(Msg))};
unpack(?MSG_CHANNELREMOVE, Msg) ->
    {channelremove,proplist(mumble_pb:decode_channelremove(Msg))};
unpack(?MSG_USERSTATE, Msg) ->
    {userstate,proplist(mumble_pb:decode_userstate(Msg))};
unpack(?MSG_USERREMOVE, Msg) ->
    {userremove,proplist(mumble_pb:decode_userremove(Msg))}.

proplist(Record) ->
    record_info:record_to_proplist(Record, ?MODULE).

authenticate(PropList) ->
    record_info:proplist_to_record(PropList, authenticate, ?MODULE).

ping(PropList) ->
    record_info:proplist_to_record(PropList, ping, ?MODULE).

version(PropList) ->
    record_info:proplist_to_record(PropList, version, ?MODULE).

userstate(PropList) ->
    record_info:proplist_to_record(PropList, userstate, ?MODULE).

userremove(PropList) ->
    record_info:proplist_to_record(PropList, userremove, ?MODULE).

channelstate(PropList) ->
    record_info:proplist_to_record(PropList, channelstate, ?MODULE).

channelremove(PropList) ->
    record_info:proplist_to_record(PropList, channelremove, ?MODULE).

cryptsetup(PropList) ->
    record_info:proplist_to_record(PropList, cryptsetup, ?MODULE).

codecversion(PropList) ->
    record_info:proplist_to_record(PropList, codecversion, ?MODULE).

serverconfig(PropList) ->
    record_info:proplist_to_record(PropList, serverconfig, ?MODULE).

serversync(PropList) ->
    record_info:proplist_to_record(PropList, serversync, ?MODULE).

permissionquery(PropList) ->
    record_info:proplist_to_record(PropList, permissionquery, ?MODULE).

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


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle(<<Type:16/unsigned-big-integer, Len:32/unsigned-big-integer, Msg:Len/binary, Rest/binary>>,
       Client) ->
    handle_pb(Type,Msg,Client),
    handle(Rest,Client);

handle(<<>>,_Client) -> 
    ok;

handle(Msg,Client) when is_tuple(Msg) ->
    MsgType = element(1,Msg),
    send(MsgType,Msg,Client).




%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
send(userstate,US,{Pid,_Key,_From}) ->
    R=mumble_pb:encode_userstate(US),
    erlmur_client:send(Pid,encode_message(?MSG_USERSTATE,R));

send(channelstate,CS,{Pid,_Key,_From}) ->
    R=mumble_pb:encode_channelstate(CS),
    erlmur_client:send(Pid,encode_message(?MSG_CHANNELSTATE,R));

send(channelremove,C,{Pid,_Key,_From}) ->
    R=mumble_pb:encode_channelremove(C),
    erlmur_client:send(Pid,encode_message(?MSG_CHANNELREMOVE,R));

send(userremove,UR,{Pid,_Key,_From}) ->
    R=mumble_pb:encode_userremove(UR),
    erlmur_client:send(Pid,encode_message(?MSG_USERREMOVE,R));

send(udp_tunnle,{_,Data},{Pid,_Key,_From}) ->
    erlmur_client:send(Pid,encode_message(?MSG_UDPTUNNEL,Data)).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_udp(<<1:3,0:5,_Timestamp/binary>>=PingMsg, {Pid,_Key,_From}) ->
    erlmur_client:send_udp(Pid,PingMsg);
handle_udp(<<Type:3,Target:5,Rest/binary>>, {Pid,_Key,_From}) ->
    {Counter,R} = erlmur_varint:decode(Rest),
    {Voice,Positional} = split_voice_positional(Type,R),
    erlmur_server:voice_data(Type,Target,Pid,Counter,Voice,Positional).



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_pb(?MSG_VERSION,_Msg,{Pid,_Key,_From}) ->
    R=mumble_pb:encode_version(erlmur_server:version()),
    erlmur_client:send(Pid,encode_message(?MSG_VERSION,R));

handle_pb(?MSG_AUTHENTICATE,Msg,{Pid,Key,{Address,_Port, Cert}}) ->
    {_,U,P,_T,C,_O} = mumble_pb:decode_authenticate(Msg),
    Sid=erlmur_server:authenticate(U,P,Address),
    CS = erlmur_channels:all_channel_states(), 
    lists:foreach(fun(K) ->
			  R=mumble_pb:encode_channelstate(K),
			  erlmur_client:send(Pid,encode_message(?MSG_CHANNELSTATE,R))
		  end, CS),
    US=erlmur_users:all_user_states(),
    lists:foreach(fun(K) ->
			  R=mumble_pb:encode_userstate(K),
			  erlmur_client:send(Pid,encode_message(?MSG_USERSTATE,R))
		  end, US),
    
    {K,DIV,EIV} = ocb128crypt:key(Key),
    CryptSetup = mumble_pb:encode_cryptsetup(#cryptsetup{key=K,
							 client_nonce=DIV,
							 server_nonce=EIV}),
    erlmur_client:send(Pid,encode_message(?MSG_CRYPTSETUP, CryptSetup)),
    
    erlmur_server:codecversion(C),
    V=mumble_pb:encode_codecversion(erlmur_server:codecversion()),
    erlmur_client:send(Pid,encode_message(?MSG_CODECVERSION, V)),
    
    Config=mumble_pb:encode_serverconfig(erlmur_server:serverconfig()),
    erlmur_client:send(Pid,encode_message(?MSG_SERVERCONFIG,Config)),
    
    R=mumble_pb:encode_serversync(erlmur_server:serversync(Sid)),
    erlmur_client:send(Pid,encode_message(?MSG_SERVERSYNC,R));

handle_pb(?MSG_PERMISSIONQUERY,Msg,{Pid,_Key,_From}) ->
    P=mumble_pb:decode_permissionquery(Msg),
    R=mumble_pb:encode_permissionquery(erlmur_server:permissionquery(P)),
    erlmur_client:send(Pid,encode_message(?MSG_PERMISSIONQUERY,R));

handle_pb(?MSG_PING,Msg,{Pid,Key,_From}) ->
    PingMsg = mumble_pb:decode_ping(Msg),
    {Good,Late,Lost,Resync} = ocb128crypt:local(Key),
    Pong = PingMsg#ping{good=Good,
			late=Late,
			lost=Lost,
			resync=Resync},
    erlmur_client:update_key_remote(Pid,{PingMsg#ping.good,
					 PingMsg#ping.late,
					 PingMsg#ping.lost,
					 PingMsg#ping.resync}),
    erlmur_client:send(Pid,encode_message(?MSG_PING,mumble_pb:encode(Pong)));

handle_pb(?MSG_CRYPTSETUP,Msg,{Pid,Key,_From}) ->
    case mumble_pb:decode_cryptsetup(Msg) of
	#cryptsetup{client_nonce = undefined} = CryptSetup ->
	    error_logger:info_report([{resync,Pid},{cryptsetup,CryptSetup}]),
	    {_K,_DIV,EIV} = ocb128crypt:key(Key),
	    CryptSetupMsg = mumble_pb:encode_cryptsetup(CryptSetup#cryptsetup{server_nonce=EIV}),
	    erlmur_client:send(Pid,encode_message(?MSG_CRYPTSETUP, CryptSetupMsg));
	CryptSetup ->
	    error_logger:info_report([{resync,Pid},{cryptsetup,CryptSetup}]),
	    erlmur_client:resync(Pid,CryptSetup#cryptsetup.client_nonce)
    end;

handle_pb(?MSG_UDPTUNNEL, Msg, {Pid,_Key,_From,_Cert}=Client) ->
    erlmur_client:udp_tunnel(Pid),
    handle_udp(Msg,Client);

handle_pb(?MSG_CHANNELSTATE, Msg, _Client) ->
    erlmur_server:channelstate(mumble_pb:decode_channelstate(Msg));

handle_pb(?MSG_CHANNELREMOVE, Msg, _Client) ->
    erlmur_server:channel_remove(mumble_pb:decode_channelremove(Msg));

handle_pb(?MSG_USERSTATE, Msg, _Client) ->
    erlmur_server:userstate(mumble_pb:decode_userstate(Msg));

handle_pb(?MSG_USERREMOVE, Msg, _Client) ->
    erlmur_server:userremove(mumble_pb:decode_userremove(Msg));

handle_pb(Type,Msg,Client) ->
    error_logger:error_report([{erlmur_message,"Unhandled message"},
			       {type, Type},
			       {msg,Msg},
			       {client,Client}]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
