-module(erlmur_session_store).

-include("erlmur.hrl").

-export([init/1, get/1]).

init(Nodes) ->
    mnesia:create_table(
        erlmur_session,
        [
            {attributes, record_info(fields, erlmur_session)},
            {ram_copies, Nodes},
            {index, [#erlmur_session.id]},
            {type, set}
        ]
    ),
    [erlmur_session].

get(SessionId) ->
    F = fun() -> mnesia:read(erlmur_session, SessionId) end,
    [S] = mnesia:activity(transaction, F),
    S.
