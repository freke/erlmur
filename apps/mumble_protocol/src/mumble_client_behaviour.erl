-module(mumble_client_behaviour).

-moduledoc """
Behaviour for implementing Mumble client logic.

Implementing this behaviour allows you to handle incoming Mumble protocol messages
and manage the internal state of a Mumble client connection.

## Implementing the Behaviour

```erlang
-module(my_client_handler).
-behaviour(mumble_client_behaviour).

-export([init/1, handle_msg/2]).

init(Opts) ->
    {ok, #{connected => false}}.

handle_msg(#{message_type := 'ServerSync'} = Msg, State) ->
    io:format("Connected with session ~p~n", [maps:get(session, Msg)]),
    {ok, State#{connected => true}};
handle_msg(_Msg, State) ->
    {ok, State}.
```

## Callbacks

- `init/1`: Initialize client state
- `handle_msg/2`: Process incoming messages
""".

-doc """
Initialize the client handler state.

Called when a client connection starts. Use this to set up initial state
and perform any necessary setup.

Input: `Opts` - Options passed from `start_client/5` (includes `callback_args`).
Output: 
  - `{ok, State}` - Initialization successful
  - `{error, Reason}` - Initialization failed
""".
-callback init(Opts :: any()) ->
    {ok, State :: any()} | {error, Reason :: any()}.

-doc """
Handle an incoming protocol message from the server.

Called for each message received from the server. This includes:
- `ServerSync` - Initial connection established
- `UserState` - User information updates
- `ChannelState` - Channel information updates
- `TextMessage` - Chat messages
- `Ping` - Server ping
- And many more

Input:
  - `Msg` - Map containing the message with `message_type` field
  - `State` - Current handler state

Output:
  - `{ok, NewState}` - Message handled, continue connection
  - `{stop, Reason, NewState}` - Close the connection
""".
-callback handle_msg(Msg :: map(), State :: any()) ->
    {ok, NewState :: any()} |
    {stop, Reason :: any(), NewState :: any()}.
