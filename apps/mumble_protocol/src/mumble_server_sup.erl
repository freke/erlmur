-module(mumble_server_sup).

-moduledoc """
Supervisor for the Mumble server application.

This module implements the top-level supervisor that manages the server
architecture. It supervises two child processes:

1. **UDP Server** (`mumble_udp_server`) - Handles voice traffic over UDP
2. **TCP Listener** (Ranch) - Accepts TLS connections for control protocol

The supervisor uses a `one_for_one` restart strategy, meaning if one child
crashes, only that child is restarted.

## Usage

This module is not typically called directly. Use `mumble:start_server/1,2,3`
instead, which will start this supervisor.
""".

-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([start_tcp_listener/5]).
%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

-doc """
Start the Mumble server supervisor.

Starts the supervisor with the given options, which will in turn start
the UDP server and TCP listener.

Input:
  - `Options` - Proplist containing:
    - `port` - Port number to listen on
    - `cert_file` - Path to TLS certificate
    - `key_file` - Path to TLS private key
    - `server_handler` - Module implementing `mumble_server_behaviour`
    - `listener_ref` - Unique reference for the Ranch listener

Output: `{ok, pid()}` on success, `{error, term()}` on failure.
""".
-spec start_link([{atom(), term()}]) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    supervisor:start_link(?MODULE, Options).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Options) ->
    Port = proplists:get_value(port, Options),
    CertFile = proplists:get_value(cert_file, Options),
    KeyFile = proplists:get_value(key_file, Options),
    ServerHandler = proplists:get_value(server_handler, Options, mock_mumble_handler),
    ListenerRef = proplists:get_value(listener_ref, Options, mumble_tcp),
    logger:debug("[mumble_server_sup] Starting on port ~p with handler: ~s", [Port, ServerHandler]),

    UdpServer = #{
        id => mumble_udp_server,
        start => {mumble_udp_server, start_link, [Port]},
        restart => permanent,
        type => worker
    },
    RanchListener = #{
        id => mumble_tcp_listener,
        start => {mumble_server_sup, start_tcp_listener, [ListenerRef, CertFile, KeyFile, Port, ServerHandler]},
        restart => permanent,
        type => supervisor
    },
    logger:debug("[mumble_server_sup] Starting UDP server..."),
    logger:debug("[mumble_server_sup] Starting TCP listener..."),
    {ok, {{one_for_one, 5, 10}, [UdpServer, RanchListener]}}.

start_tcp_listener(Ref, CertFile, KeyFile, Port, Handler) ->
    logger:debug("[mumble_server_sup] Initializing TLS with cert: ~s", [CertFile]),
    ranch:start_listener(
        Ref,
        ranch_ssl,
        #{
            socket_opts => [
                {port, Port},
                {certfile, CertFile},
                {keyfile, KeyFile}
            ],
            num_acceptors => 10
        },
        mumble_server_conn,
        [Handler]
    ).
