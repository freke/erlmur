-module(mumble_client).

-moduledoc """
Client operations module for Mumble protocol.

This module handles client connection lifecycle including connecting to servers,
sending messages, receiving messages, and managing client state.

This is an internal module - use mumble.erl for the public API.
""".

-export([start_client/5, stop_client/1, send/2, send_voice/2, get_state/1]).

-include("mumble_protocol.hrl").

-doc """
Start a Mumble client connection to a server.

Input:
  - CertFile: Path to TLS client certificate (or undefined for no cert)
  - KeyFile: Path to TLS client private key (or undefined for no key)
  - Host: Server hostname or IP address
  - Port: Server port number
  - Options: Map of client options including:
    - username: Username for authentication
    - password: Password for authentication
    - callback: Module implementing mumble_client_behaviour
    - callback_args: Arguments for callback init

Returns:
  - {ok, client_ref()}: Client connected successfully
  - {error, term()}: Connection failed

The client_ref is an opaque reference used for subsequent operations.
""".
-spec start_client(
    file:filename_all() | undefined,
    file:filename_all() | undefined,
    string(),
    inet:port_number(),
    map()
) -> {ok, mumble:client_ref()} | {error, term()}.
start_client(CertFile, KeyFile, Host, Port, Options) ->
    %% Validate files if provided
    case {CertFile, KeyFile} of
        {undefined, undefined} ->
            %% No TLS files, use default SSL options
            start_client_impl(Host, Port, Options);
        {CF, KF} ->
            %% Validate TLS files
            case mumble_cert:validate_cert_files(CF, KF) of
                ok ->
                    %% Add TLS files to options
                    OptsWithCerts = Options#{
                        cert_file => CF,
                        key_file => KF
                    },
                    start_client_impl(Host, Port, OptsWithCerts);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-doc """
Stop a Mumble client connection and clean up resources.

Input: Client reference obtained from start_client/5.

Returns:
  - ok: Client stopped successfully
  - {error, term()}: Failed to stop client
""".
-spec stop_client(mumble:client_ref()) -> ok | {error, term()}.
stop_client({mumble_client, Pid}) ->
    case mumble_client_conn:stop(Pid) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end;
stop_client(_) ->
    {error, invalid_client_ref}.

-doc """
Send a message to the Mumble server through the client connection.

Input:
  - ClientRef: Client reference from start_client/5
  - Msg: Message map to send

Returns:
  - ok: Message sent successfully
  - {error, term()}: Failed to send message
""".
-spec send(mumble:client_ref(), map()) -> ok | {error, term()}.
send({mumble_client, Pid}, Msg) ->
    mumble_client_conn:send(Pid, Msg);
send(_, _) ->
    {error, invalid_client_ref}.

-doc """
Send voice data to the Mumble server through the client connection.

Input:
  - ClientRef: Client reference from start_client/5
  - VoiceData: Voice data tuple {voice_data, Type, Target, Counter, Voice, Positional}

Returns:
  - ok: Voice data sent successfully
  - {error, term()}: Failed to send voice data
""".
-spec send_voice(mumble:client_ref(), {voice_data, non_neg_integer(), non_neg_integer(), non_neg_integer(), binary(), any()}) ->
    ok | {error, term()}.
send_voice({mumble_client, Pid}, Msg) ->
    mumble_client_conn:send_voice(Pid, Msg);
send_voice(_, _) ->
    {error, invalid_client_ref}.

-doc """
Get the current state of the client connection.

Input: Client reference from start_client/5.

Returns:
  - {ok, term()}: Current client state
  - {error, term()}: Failed to get state
""".
-spec get_state(mumble:client_ref()) -> {ok, term()} | {error, term()}.
get_state({mumble_client, Pid}) ->
	{ok, mumble_client_conn:get_state(Pid)};
get_state(_) ->
    {error, invalid_client_ref}.

%% Internal functions

-spec start_client_impl(string(), inet:port_number(), map()) ->
    {ok, mumble:client_ref()} | {error, term()}.
start_client_impl(Host, Port, Options) ->
    %% Start client connection
    case mumble_client_conn:start_link(Host, Port, Options) of
        {ok, Pid} ->
            %% Give the process a moment to attempt connection
            timer:sleep(100),
            %% Check if process is still alive (connection may have failed)
            case is_process_alive(Pid) of
                true ->
                    ClientRef = {mumble_client, Pid},
                    {ok, ClientRef};
                false ->
                    %% Process died, likely due to connection failure
                    {error, connection_failed}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
