-module(erlmur_authenticate).

-export([check/2]).

check(Username, _Password) ->
    {ok, erlmur_user_store:add(Username)}.
