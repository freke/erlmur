# Mumble Protocol

This library implements the Mumble protocol for Erlang,
providing a robust foundation for both **Server** and **Client** implementations.

## Features

- **Dual Role Support**: Built-in connection handlers for both Mumble servers and clients.
- **TCP Protocol**: Full packing and unpacking of Protobuf-defined messages.
- **UDP Voice**: Handling of the UDP-based voice channel, including OCB-AES128 encryption integration.
- **UDPTunneling**: Support for tunneling UDP payloads (voice) within the TCP stream to bypass restrictive
  firewalls.
- **Voice Fallback**: Intelligent, automatic fallback to TCP-tunneled voice when UDP is unverified or connection
  quality drops.
- **Ping/Pong**: Integrated heartbeats for both TCP and UDP to maintain connection liveness and measure latency.
- **Varint Support**: Correct implementation of Mumble's custom variable-length integer encoding.
- **Version Management**: Utilities for encoding and comparing Mumble protocol versions.

## Protocol Details

### Ping and Heartbeats

The protocol uses three types of pings:

1. **TCP Pings**: Used to keep the connection alive and measure the round-trip time (RTT). The server sends periodic
   pings, and the client responds with its own statistics (packet counts).
2. **Encrypted UDP Pings**: Used primarily to verify UDP connectivity. The client sends UDP pings to the server's UDP port.
   If the server receives one and is able to decrypt it, it responds, and the connection is marked as "UDP verified" true.
3. **Unencrypted UDP Ping**: Used before the client is connected to see numbers of users and latency.

If no `Ping` is recieved for 30sec the client will be disconnected from the server.

### UDPTunneling

When UDP is unavailable or unreliable, Mumble allows "tunneling" UDP-like packets (voice data and pings) through the
existing TCP connection. This is handled by wrapping the UDP payload in a `UDPTunnel` Protobuf message.
If reciveing a `UDPTunnel` message the connection is marked "UDP verified" false, until new UDP verification is done.
UDP connectivity can be disabled in the config, if so all voice messages will be "tunneling".

### Voice Fallback

The library implements an automatic fallback mechanism:

- **UDP First**: The system prefers UDP for voice to ensure the lowest possible latency.
- **Automatic Fallback**: If a server hasn't received a UDP packet from a client recently (or if the client explicitely
  sends voice via TCP), the server will switch to sending that client's voice data through the TCP tunnel.
- **Recovery**: Once a valid UDP packet is received again, the system can transparently switch back to UDP for that
  session.

## Usage

### Server Implementation

To build a server, implement the `mumble_server_behaviour` and use `mumble_server_conn` as your `ranch` protocol
handler.

**Starting the Server:**

```erlang
% Example: Starting a server with Ranch
mumble:start_server({certfile, "cert.pem"}, {keyfile, "key.pem"}, 64738).
```

**Receiving and Sending Messages:**

The server calls `handle_msg/2` on your handler module for every incoming TCP message.

```erlang
-module(my_handler_mod).
-behaviour(mumble_server_behaviour).

% ... init and other callbacks ...

handle_msg(#'TextMessage'{message = <<"ping">>}, State) ->
    % Send a message back to the client
    % The server connection process (self() in this context) can be reached by 
    % storing it in your state or using a known registered name if applicable, 
    % but usually you have the connection PID.
    mumble_server_conn:send(self(), #'TextMessage'{message = <<"pong">>}),
    {ok, State};
handle_msg(_Msg, State) ->
    {ok, State}.
```

### Client Implementation

To build a client, start a connection using `mumble_client_conn:start_link/3`. The client process will forward all
received Mumble messages to its parent process.

**Connecting and Sending:**

```erlang
% Example: Starting a client
Opts = [{certfile, "client_cert.pem"}, {keyfile, "client_key.pem"}],
{ok, ClientPid} = mumble_client_conn:start_link({127, 0, 0, 1}, 64738, Opts).

% Sending a TCP message
mumble_client_conn:send(ClientPid, #'TextMessage'{message = <<"Hello Server!">>}).

% Sending voice data (binary)
% Packet: {voice_data, Type, Target, Counter, VoiceBin, PositionalBin}
Voice = {voice_data, 0, 0, 1, <<"audio bytes">>, undefined},
mumble_client_conn:send_voice(ClientPid, Voice).
```

**Receiving Messages:**

The client forwards all decoded messages to the process that started it (its parent) as `{mumble_msg, Record}`.

```erlang
% Example: Receiving in the parent process
loop() ->
    receive
        {mumble_msg, #'TextMessage'{message = Msg}} ->
            io:format("Received text: ~s~n", [Msg]),
            loop();
        {mumble_msg, #'ServerSync'{} = Sync} ->
            io:format("Logged in. My session ID is ~p~n", 
                      [Sync#'ServerSync'.session]),
            loop();
        _Other ->
            loop()
    end.
```

## Documentation

Comprehensive guides are available in the `docs/` folder:

- [Establishing a Connection](docs/establishing_connection.md):
  A detailed walkthrough of the Mumble handshake and synchronization phase.
