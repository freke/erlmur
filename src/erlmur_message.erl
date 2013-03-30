%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 17 Mar 2013 by  <davabe@hotmail.com>

-module(erlmur_message).

-export([handle/2,handle_udp/2]).

-include("mumble_pb.hrl").

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

handle({userstate,UR},{Pid,_Key,_From}) ->
    R=mumble_pb:encode_userstate(UR),
    erlmur_client:send(Pid,encode_message(?MSG_USERSTATE,R));

handle({deluser,UR},{Pid,_Key,_From}) ->
    R=mumble_pb:encode_userremove(UR),
    erlmur_client:send(Pid,encode_message(?MSG_USERREMOVE,R));

handle({udp_tunnle,Data},{Pid,_Key,_From}) ->
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
    {Voice,Positional} = case Type of
			     4 -> {R,<<>>};
			     _ -> voice_data(R)
			 end,
    erlmur_server:voice_data(Type,Target,Pid,Counter,Voice,Positional).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
voice_data(<<1:1,Len:7,V1:Len/binary,Rest/binary>>) ->
    {V2,R1} = voice_data(Rest),
    {<<1:1,Len:7,V1:Len/binary,V2/binary>>,R1};
voice_data(<<0:1,Len:7,V:Len/binary,Rest/binary>>) ->
    {<<0:1,Len:7,V:Len/binary>>,Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_pb(?MSG_VERSION,_Msg,{Pid,_Key,_From}) ->
    R=mumble_pb:encode_version(erlmur_server:version()),
    erlmur_client:send(Pid,encode_message(?MSG_VERSION,R));

handle_pb(?MSG_AUTHENTICATE,Msg,{Pid,Key,{Address,_Port}}) ->
    {_,U,P,_T,C,_O} = mumble_pb:decode_authenticate(Msg),
    Sid=erlmur_server:authenticate(U,P,Address),
    CS=erlmur_server:channelstates(),
    lists:foreach(fun(K) ->
			  R=mumble_pb:encode_channelstate(dict:fetch(K,CS)),
			  erlmur_client:send(Pid,encode_message(?MSG_CHANNELSTATE,R))
		  end, dict:fetch_keys(CS)),
    US=erlmur_server:userstates(),
    lists:foreach(fun(K) ->
			  R=mumble_pb:encode_userstate(dict:fetch(K,US)),
			  erlmur_client:send(Pid,encode_message(?MSG_USERSTATE,R))
		  end, dict:fetch_keys(US)),
    
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
    erlmur_client:session_id(Pid,Sid),
    erlmur_client:send(Pid,encode_message(?MSG_SERVERSYNC,R));

handle_pb(?MSG_PERMISSIONQUERY,Msg,{Pid,_Key,_From}) ->
    P=mumble_pb:decode_permissionquery(Msg),
    R=mumble_pb:encode_permissionquery(erlmur_server:permissionquery(P)),
    erlmur_client:send(Pid,encode_message(?MSG_PERMISSIONQUERY,R));

handle_pb(?MSG_PING,Msg,{Pid,Key,_From}) ->
    PingMsg = mumble_pb:decode_ping(Msg),
    {Good,Late,Lost,Resync} = ocb128crypt:local(Key),
    NewKey = ocb128crypt:remote(PingMsg#ping.good,
				PingMsg#ping.late,
				PingMsg#ping.lost,
				PingMsg#ping.resync,
				Key),
    Pong = PingMsg#ping{good=Good,
			late=Late,
			lost=Lost,
			resync=Resync},
    erlmur_client:newkey(Pid,NewKey),
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
	    NewKey = ocb128crypt:client_nonce(CryptSetup#cryptsetup.client_nonce,Key),
	    erlmur_client:newkey(Pid,NewKey)
    end;

handle_pb(?MSG_UDPTUNNEL,Msg,{Pid,_Key,_From}=Client) ->
    erlmur_client:udp_tunnel(Pid),
    handle_udp(Msg,Client);

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
encode_message(Type, Msg) ->
    BMsg = erlang:iolist_to_binary(Msg),
    Len = byte_size(BMsg),
    <<Type:16/unsigned-big-integer, Len:32/unsigned-big-integer,BMsg/binary>>.





