-module(mumble_server).

-moduledoc """
Server operations module for Mumble protocol.

This module handles server lifecycle management including starting servers,
stopping servers, and retrieving server configuration.

This is an internal module - use mumble.erl for the public API.
""".

-export([start_server/1, stop_listener/1, server_version/0, serverconfig/0]).

-include("mumble_protocol.hrl").

-define(DEFAULT_PORT, 64738).

-doc """
Start a Mumble server with the given options.

Input: Options map containing:
  - cert_file: Path to TLS certificate file
  - key_file: Path to TLS private key file
  - port: Port number to listen on (default: 64738)
  - server_handler: Module implementing mumble_server_behaviour

Returns:
  - {ok, server_ref()}: Server started successfully
  - {error, term()}: Server failed to start

The server_ref is an opaque reference that can be used to stop the server.
""".
-spec start_server(map()) -> {ok, mumble:server_ref()} | {error, term()}.
start_server(Options) ->
    CertFile = maps:get(cert_file, Options, undefined),
    KeyFile = maps:get(key_file, Options, undefined),
    Port = maps:get(port, Options, ?DEFAULT_PORT),
    ServerHandler = maps:get(server_handler, Options, mock_mumble_handler),

    %% Validate certificate files
    case mumble_cert:validate_cert_files(CertFile, KeyFile) of
        ok ->
            start_server_with_certs(CertFile, KeyFile, Port, ServerHandler);
        {error, Reason} ->
            {error, Reason}
    end.

-doc """
Stop a running Mumble server.

Input: Server reference obtained from start_server/1.

Returns:
  - ok: Server stopped successfully
  - {error, term()}: Failed to stop server
""".
-spec stop_listener(mumble:server_ref()) -> ok | {error, term()}.
stop_listener({mumble_server, SupPid, ListenerRef}) ->
    %% Stop the ranch listener first
    ok = ranch:stop_listener(ListenerRef),
    %% Stop the supervisor
    case supervisor:terminate_child(SupPid, mumble_udp_server) of
        ok ->
            supervisor:terminate_child(SupPid, mumble_tcp_listener),
            exit(SupPid, normal),
            ok;
        {error, not_found} ->
            %% Already stopped or never started
            exit(SupPid, normal),
            ok;
        {error, Reason} ->
            {error, Reason}
    end;
stop_listener(_) ->
    %% Invalid reference
    {error, invalid_server_ref}.

-doc """
Get the server version information for protocol responses.

Returns version information used in server ping responses and protocol handshake.
The version record includes major/minor/patch numbers and platform information.

Output: #version{} record containing major, minor, patch, release, os, and os_version.
""".
-spec server_version() -> #version{}.
server_version() ->
    #version{
        major = ?MUMBLE_PROTOCOL_VERSION_MAJOR,
        minor = ?MUMBLE_PROTOCOL_VERSION_MINOR,
        patch = ?MUMBLE_PROTOCOL_VERSION_PATCH,
        release = <<"Erlmur">>,
        os = <<"Erlang/OTP">>,
        os_version = erlang:system_info(otp_release)
    }.

-doc """
Get the server configuration for protocol responses.

Returns server configuration used in ping responses to advertise capabilities
to clients. Includes settings like max_clients and max_bandwidth.

Output: #server_config{} record containing server configuration.
""".
-spec serverconfig() -> #server_config{}.
serverconfig() ->
    #server_config{}.

%% Internal functions

-spec start_server_with_certs(file:filename_all(), file:filename_all(), inet:port_number(), module()) ->
    {ok, mumble:server_ref()} | {error, term()}.
start_server_with_certs(CertFile, KeyFile, Port, ServerHandler) ->
    %% Create a unique listener reference
    ListenerRef = make_ref(),

    SupOptions = [
        {port, Port},
        {cert_file, CertFile},
        {key_file, KeyFile},
        {server_handler, ServerHandler},
        {listener_ref, ListenerRef}
    ],

    case mumble_server_sup:start_link(SupOptions) of
        {ok, SupPid} ->
            {ok, {mumble_server, SupPid, ListenerRef}};
        {error, Reason} ->
            {error, {supervisor_start_failed, Reason}}
    end.
