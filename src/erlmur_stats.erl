-module(erlmur_stats).

-moduledoc "Manages and tracks various statistics for client sessions.\n\nThis "
"module provides functions to update and retrieve statistics "
"related to network\npackets, ping times, and other session-specific "
"metrics.".

-export([new/0, times/2, packets/2, client_stats/2, server_stats/2]).

-include("erlmur.hrl").

-opaque stats() :: #stats{}.

-export_type([stats/0]).

-opaque ping() :: #ping{}.

-export_type([ping/0]).

new() ->
    #stats{}.

times({UdpPingAvg, UdpPingVar, TcpPingAvg, TcpPingVar}, Stats) ->
    Stats#stats{
        udp_ping_avg = UdpPingAvg,
        udp_ping_var = UdpPingVar,
        tcp_ping_avg = TcpPingAvg,
        tcp_ping_var = TcpPingVar
    }.

packets({UdpPackets, TcpPackets}, Stats) ->
    Stats#stats{udp_packets = UdpPackets, tcp_packets = TcpPackets}.

client_stats({Good, Late, Lost, Resync}, Stats) ->
    ClientPing = Stats#stats.client_ping,
    NewClientPing =
        ClientPing#ping{
            good = Good,
            late = Late,
            lost = Lost,
            resync = Resync
        },
    logger:debug("ClientPing ~p", [NewClientPing]),
    Stats#stats{client_ping = NewClientPing}.

server_stats({stats, Good, Late, Lost}, Stats) ->
    ServerPing = Stats#stats.server_ping,
    NewServerPing =
        ServerPing#ping{
            good = ServerPing#ping.good + Good,
            late = ServerPing#ping.late + Late,
            lost = ServerPing#ping.lost + Lost
        },
    logger:debug("ServerPing ~p", [NewServerPing]),
    Stats#stats{server_ping = NewServerPing};
server_stats(
    {from_client_udp, Bytes},
    Stats = #stats{
        from_client_udp_bytes = OldBytes, from_client_udp_packets = UdpPackets
    }
) ->
    Stats#stats{from_client_udp_bytes = OldBytes + Bytes, from_client_udp_packets = UdpPackets + 1};
server_stats(
    {from_client_tcp, Bytes},
    Stats = #stats{
        from_client_tcp_bytes = OldBytes, from_client_tcp_packets = TcpPackets
    }
) ->
    Stats#stats{from_client_tcp_bytes = OldBytes + Bytes, from_client_tcp_packets = TcpPackets + 1}.
