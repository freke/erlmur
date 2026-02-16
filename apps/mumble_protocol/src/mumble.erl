-module(mumble).

-moduledoc """
Main public API module for the Mumble protocol implementation.

This module provides high-level functions for starting Mumble servers and
client connections. All implementation logic is delegated to specialized modules:

- `mumble_server`: Server lifecycle operations
- `mumble_client`: Client connection operations
- `mumble_cert`: Certificate management

## Server API

Start a server with auto-generated certificates:
```erlang
{ok, ServerRef} = mumble:start_server(#{
    port => 64738,
    auto_create_cert => true
}).
```

Start a server with existing certificates:
```erlang
{ok, ServerRef} = mumble:start_server(#{
    cert_file => "/path/to/cert.pem",
    key_file => "/path/to/key.pem",
    port => 64738
}).
```

## Client API

Connect to a Mumble server:
```erlang
{ok, ClientRef} = mumble:start_client(undefined, undefined, "localhost", 64738).
```
""".

%% Public API exports
-export([start_server/1, start_server/2, start_server/3, start_server/4, stop_listener/1]).
-export([start_client/4, start_client/5, stop_client/1, send/2, send_voice/2, get_state/1, join_channel/2]).
-export([server_version/0, serverconfig/0]).

-include("mumble_protocol.hrl").

-define(DEFAULT_PORT, 64738).
-define(DEFAULT_CERT_DAYS, 365).
-define(DEFAULT_CERT_SUBJECT, "/CN=localhost").

%% Type definitions

-doc """
Options for configuring a Mumble server.

Available options:
- `cert_file`: Path to TLS certificate file
- `key_file`: Path to TLS private key file
- `port`: Port number to listen on (default: 64738)
- `auto_create_cert`: Whether to auto-generate self-signed certificates
- `cert_subject`: Subject line for auto-generated certificates
- `cert_days`: Validity period in days for auto-generated certificates
- `server_handler`: Module implementing `mumble_server_behaviour`
""".
-type server_options() :: #{
    cert_file => file:filename_all(),
    key_file => file:filename_all(),
    port => inet:port_number(),
    auto_create_cert => boolean(),
    cert_subject => string(),
    cert_days => pos_integer(),
    server_handler => module()
}.

-doc """
Reference to a running Mumble server.

This opaque reference is returned by `start_server/1,2,3` and must be passed
to `stop_listener/1` to shut down the server.
""".
-type server_ref() :: {mumble_server, pid(), reference()}.

-doc """
Reference to an active Mumble client connection.

This opaque reference is returned by `start_client/4,5` and is used to
interact with the connection (send messages, query state, etc.).
""".
-type client_ref() :: {mumble_client, pid()}.

-doc """
Options for configuring a Mumble client connection.

Available options:
- `cert_file`: Path to TLS client certificate file
- `key_file`: Path to TLS client private key file
- `username`: Username for authentication
- `password`: Password for authentication
- `callback`: Module implementing `mumble_client_behaviour` for message handling
- `callback_args`: Arguments passed to the callback module's `init/1`
""".
-type client_options() :: #{
    cert_file => file:filename_all(),
    key_file => file:filename_all(),
    username => string(),
    password => string(),
    callback => module(),
    callback_args => list()
}.

%% Export types for use by other modules
-export_type([server_options/0, server_ref/0, client_ref/0, client_options/0]).

%% =============================================================================
%% Server API
%% =============================================================================

-doc """
Start a Mumble server with configuration options.

Input: Options map containing server configuration (cert_file, key_file, port, auto_create_cert, etc.).
Output: {ok, server_ref()} on success, {error, term()} on failure.
""".
-spec start_server(server_options()) -> {ok, server_ref()} | {error, term()}.
start_server(Options) when is_map(Options) ->
    CertFile = maps:get(cert_file, Options, undefined),
    KeyFile = maps:get(key_file, Options, undefined),
    Port = maps:get(port, Options, ?DEFAULT_PORT),
    AutoCreate = maps:get(auto_create_cert, Options, false),
    ServerHandler = maps:get(server_handler, Options, mock_mumble_handler),

    case {CertFile, KeyFile, AutoCreate} of
        {undefined, undefined, true} ->
            %% Auto-generate certificates
            CertSubject = maps:get(cert_subject, Options, ?DEFAULT_CERT_SUBJECT),
            CertDays = maps:get(cert_days, Options, ?DEFAULT_CERT_DAYS),
            case mumble_cert:ensure_auto_certs(CertSubject, CertDays) of
                {ok, AutoCertFile, AutoKeyFile} ->
                    ServerOpts = #{
                        cert_file => AutoCertFile,
                        key_file => AutoKeyFile,
                        port => Port,
                        server_handler => ServerHandler
                    },
                    mumble_server:start_server(ServerOpts);
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            %% Use provided files
            ServerOpts = #{
                cert_file => CertFile,
                key_file => KeyFile,
                port => Port,
                server_handler => ServerHandler
            },
            mumble_server:start_server(ServerOpts)
    end.

-doc """
Start a Mumble server with certificate files (backward compatible API).
Input: Certificate file path and key file path.
Output: {ok, server_ref()} on success, {error, term()} on failure.
""".
-spec start_server(file:filename_all(), file:filename_all()) ->
    {ok, server_ref()} | {error, term()}.
start_server(CertFile, KeyFile) ->
    start_server(CertFile, KeyFile, ?DEFAULT_PORT, mock_mumble_handler).

-doc """
Start a Mumble server with certificate files and port number.
Input: Certificate file path, key file path, and port number.
Output: {ok, server_ref()} on success, {error, term()} on failure.
""".
-spec start_server(file:filename_all(), file:filename_all(), inet:port_number()) ->
    {ok, server_ref()} | {error, term()}.
start_server(CertFile, KeyFile, Port) ->
    start_server(CertFile, KeyFile, Port, mock_mumble_handler).

-doc """
Start a Mumble server with certificate files, port, and custom handler.

This is the full-arity version providing complete control over server configuration.
The handler module must implement the `mumble_server_behaviour` callbacks.

Input:
  - `CertFile`: Path to TLS certificate file
  - `KeyFile`: Path to TLS private key file
  - `Port`: Port number to listen on
  - `ServerHandler`: Module implementing `mumble_server_behaviour`

Output: {ok, server_ref()} on success, {error, term()} on failure.
""".
-spec start_server(file:filename_all(), file:filename_all(), inet:port_number(), module()) ->
    {ok, server_ref()} | {error, term()}.
start_server(CertFile, KeyFile, Port, ServerHandler) ->
    ServerOpts = #{
        cert_file => CertFile,
        key_file => KeyFile,
        port => Port,
        server_handler => ServerHandler
    },
    mumble_server:start_server(ServerOpts).

-doc """
Stop a running Mumble server and clean up resources.
Input: Server reference containing supervisor PID and listener reference.
Output: ok on success, {error, term()} on failure.
""".
-spec stop_listener(server_ref()) -> ok | {error, term()}.
stop_listener(ServerRef) ->
    mumble_server:stop_listener(ServerRef).

-doc """
Get the server version information for protocol responses.

Returns version information used in server ping responses and protocol handshake.
The version record includes major/minor/patch numbers and platform information.

Output: #version{} record containing major, minor, patch, release, os, and os_version.
""".
-spec server_version() -> #version{}.
server_version() ->
    mumble_server:server_version().

-doc """
Get the server configuration for protocol responses.

Returns server configuration used in ping responses to advertise capabilities
to clients. Includes settings like max_clients and max_bandwidth.

Output: #server_config{} record containing server configuration.
""".
-spec serverconfig() -> #server_config{}.
serverconfig() ->
    mumble_server:serverconfig().

%% =============================================================================
%% Client API
%% =============================================================================

-doc """
Start a Mumble client connection to a server.
Input: Certificate file path, key file path, host string, and port number.
Output: {ok, client_ref()} on success, {error, term()} on failure.
""".
-spec start_client(file:filename_all(), file:filename_all(), string(), inet:port_number()) ->
    {ok, client_ref()} | {error, term()}.
start_client(CertFile, KeyFile, Host, Port) ->
    start_client(CertFile, KeyFile, Host, Port, #{}).

-doc """
Start a Mumble client connection with options map.
Input: Certificate file path, key file path, host string, port number, and options map.
Output: {ok, client_ref()} on success, {error, term()} on failure.
""".
-spec start_client(file:filename_all(), file:filename_all(), string(), inet:port_number(), client_options()) ->
    {ok, client_ref()} | {error, term()}.
start_client(CertFile, KeyFile, Host, Port, Options) when is_map(Options) ->
    mumble_client:start_client(CertFile, KeyFile, Host, Port, Options).

-doc """
Send a message to the Mumble server through the client connection.
Input: Client reference and message map.
Output: ok on success, {error, term()} on failure.
""".
-spec send(client_ref(), map()) -> ok | {error, term()}.
send(ClientRef, Msg) ->
    mumble_client:send(ClientRef, Msg).

-doc """
Send voice data to the Mumble server through the client connection.
Input: Client reference and voice data tuple {voice_data, Type, Target, Counter, Voice, Positional}.
Output: ok on success, {error, term()} on failure.
""".
-spec send_voice(client_ref(), {voice_data, non_neg_integer(), non_neg_integer(), non_neg_integer(), binary(), any()}) ->
    ok | {error, term()}.
send_voice(ClientRef, Msg) ->
    mumble_client:send_voice(ClientRef, Msg).

-doc """
Get the current state of the client connection.
Input: Client reference.
Output: {ok, term()} containing client state on success, {error, term()} on failure.
""".
-spec get_state(client_ref()) -> {ok, term()} | {error, term()}.
get_state(ClientRef) ->
    mumble_client:get_state(ClientRef).

-doc """
Join a channel on the Mumble server.

Input:
  - ClientRef: Client reference obtained from start_client/5
  - ChannelId: Integer channel ID to join

Output: ok on success, {error, term()} on failure.

The client sends a UserState message to the server requesting to join
the specified channel. The server will confirm with a UserState message.
""".
-spec join_channel(client_ref(), non_neg_integer()) -> ok | {error, term()}.
join_channel(ClientRef, ChannelId) ->
    mumble_client:join_channel(ClientRef, ChannelId).

-doc """
Stop a Mumble client connection and clean up resources.
Input: Client reference.
Output: ok on success, {error, term()} on failure.
""".
-spec stop_client(client_ref()) -> ok | {error, term()}.
stop_client(ClientRef) ->
    mumble_client:stop_client(ClientRef).
