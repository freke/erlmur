-module(erlmur).
-moduledoc """
Public API for the erlmur server.

This module provides a set of public functions for interacting with the erlmur
server, allowing external applications or the Erlang shell to manage the server.
""".

-export([start/0, register_user/1]).

start() ->
    application:ensure_all_started(erlmur).

register_user(_User) ->
    ok.
