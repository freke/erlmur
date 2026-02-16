-module(mumble_server_behaviour).

-moduledoc """
Behaviour definition for Mumble server handlers.

This module defines the callback interface that modules must implement
to handle Mumble protocol events. Server handlers are responsible for:

- Initializing server state
- Authenticating connecting users
- Processing incoming messages
- Providing server capability information

## Implementing the Behaviour

```erlang
-module(my_server_handler).
-behaviour(mumble_server_behaviour).

-export([init/1, authenticate/2, handle_msg/2, get_caps/1]).

init(_Opts) ->
    {ok, #{users => #{}}}.

authenticate(#{username := Username}, State) ->
    {ok, #{username => Username, session_id => 123}, State}.

handle_msg(Msg, State) ->
    {ok, State}.

get_caps(_State) ->
    #{major => 1, minor => 2, patch => 4, release => <<"MyServer">>}.
```
""".

-doc """
Initialize the server handler state.

Called when the server starts. Use this to set up any necessary state
for your application.

Input: `Opts` - Options passed from `start_server/4`.
Output: `{ok, State}` where `State` is your custom handler state.
""".
-callback init(Opts :: any()) -> {ok, State :: any()}.

-doc """
Authenticate a connecting user.

Called when a client sends an `Authenticate` message. Validate credentials
and return user information if authentication succeeds.

Input:
  - `AuthMsg` - Map containing authentication details (username, password, tokens)
  - `State` - Current handler state

Output:
  - `{ok, UserInfo, NewState}` - Authentication successful, `UserInfo` contains at least `username` and `session_id`
  - `{error, Reason}` - Authentication failed
""".
-callback authenticate(AuthMsg :: map(), State :: any()) ->
    {ok, UserInfo :: any(), NewState :: any()} | {error, Reason :: any()}.

-doc """
Handle an incoming protocol message.

Called for each message received from a connected client (except those
handled internally like `Version`, `Authenticate`, `Ping`).

Input:
  - `Msg` - Map containing the message (with `message_type` field)
  - `State` - Current handler state

Output:
  - `{ok, NewState}` - Message handled successfully
  - `{stop, Reason, NewState}` - Close the connection
""".
-callback handle_msg(Msg :: map(), State :: any()) ->
    {ok, NewState :: any()} | {stop, Reason :: any(), NewState :: any()}.

-doc """
Get server capability information.

Called to retrieve version and capability information for the protocol
handshake. Optional callback.

Input: `State` - Current handler state.
Output: Map containing at least `major`, `minor`, `patch`, and optionally
        `release`, `os`, `os_version`.

### Example Return Value
```erlang
#{
    major => 1,
    minor => 2, 
    patch => 4,
    release => <<"MyServer">>,
    os => <<"Linux">>,
    os_version => <<"5.0">>
}
```
""".
-callback get_caps(State :: any()) -> Caps :: any().

-optional_callbacks([get_caps/1]).
