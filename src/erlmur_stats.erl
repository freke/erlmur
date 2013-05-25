%%% @author  <>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 24 May 2013 by  <>

-module(erlmur_stats).

-export([new/0, client_ping/2, times/2, packets/2, server_ping/1, stats/1]).

-record(ping_stats, {good=0,late=0,lost=0,resync=0}).
-record(stats, {server_ping=#ping_stats{},
		client_ping=#ping_stats{},
		udp_packets=0,
		tcp_packets=0,
		udp_ping_avg=0,
		udp_ping_var=0,
		tcp_ping_avg=0,
		tcp_ping_var=0,
		onlinesecs=0,
		idlesecs=0}).

-include_lib("record_info/include/record_info.hrl").

-export_record_info([stats]).

new() ->
    #stats{}.

client_ping({Good,Late,Lost,Resync},Stats) ->
    ClientPing = Stats#stats.client_ping,
    NewClientPing = ClientPing#ping_stats{good=Good,late=Late,lost=Lost,resync=Resync},
    Stats#stats{client_ping=NewClientPing}.

times({UdpPingAvg,UdpPingVar,TcpPingAvg,TcpPingVar},Stats) ->
    Stats#stats{udp_ping_avg=UdpPingAvg,
		udp_ping_var=UdpPingVar,
		tcp_ping_avg=TcpPingAvg,
		tcp_ping_var=TcpPingVar}.

packets({UdpPackets,TcpPackets},Stats) ->
    Stats#stats{udp_packets=UdpPackets,
		tcp_packets=TcpPackets}.

server_ping(Stats) ->
    Server = Stats#stats.server_ping,
    {Server#ping_stats.good,Server#ping_stats.late,Server#ping_stats.lost,Server#ping_stats.resync}.


stats(Stats) ->
    [{stats_only,true}|record_info:record_to_proplist(Stats, ?MODULE)].

