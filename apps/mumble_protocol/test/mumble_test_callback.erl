-module(mumble_test_callback).
-behaviour(mumble_client_behaviour).

-export([init/1, handle_msg/2]).

init(Opts) ->
    Parent = maps:get(parent, Opts, undefined),
    {ok, #{parent => Parent, messages => []}}.

handle_msg(Msg, #{parent := Parent} = State) when is_pid(Parent) ->
    Parent ! {mumble_callback, Msg},
    {ok, State};
handle_msg(_Msg, State) ->
    {ok, State}.
