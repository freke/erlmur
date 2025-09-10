-module(erlmur_id).

-moduledoc "Manages the generation and allocation of unique IDs for sessions "
"and users.\n\nThis module ensures that each session and user "
"within the Erlmur system has a distinct identifier.".

-export([start/0, stop/0, new_session_id/0, new_user_id/0]).

-record(erlmur_id_counters, {id, value = 0}).

start() ->
    ets:new(erlmur_id_counters, [set, {keypos, #erlmur_id_counters.id}, named_table, public]),
    ets:insert(erlmur_id_counters, #erlmur_id_counters{id = session_id, value = 0}),
    ets:insert(erlmur_id_counters, #erlmur_id_counters{id = user_id, value = 0}).

stop() ->
    ets:delete(erlmur_id_counters).

new_session_id() ->
    ets:update_counter(erlmur_id_counters, session_id, {#erlmur_id_counters.value, 1}).

new_user_id() ->
    ets:update_counter(erlmur_id_counters, user_id, {#erlmur_id_counters.value, 1}).
