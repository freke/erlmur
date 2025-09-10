-module(erlmur_user_store).
-moduledoc """
Manages user state.

This module is responsible for creating, reading, updating, and deleting user
records. It also notifies other parts of the system of any changes to user state.
""".

-export([
    init/1,
    get/1,
    active_users/0,
    registered_users/0,
    users_in_channel/1,
    remove/5,
    add/1,
    update/3,
    list/0,
    move_to_channel/3
]).

-include_lib("stdlib/include/ms_transform.hrl").

-include("erlmur.hrl").

-type user() :: #user{}.
-export_type([user/0]).
-type registered_user() :: #registered_user{}.
-export_type([registered_user/0]).

init(Nodes) ->
    mnesia:create_table(
        user,
        [
            {attributes, record_info(fields, user)},
            {ram_copies, Nodes},
            {type, set}
        ]
    ),
    mnesia:create_table(
        registered_user,
        [
            {attributes, record_info(fields, registered_user)},
            {ram_copies, Nodes},
            {type, set}
        ]
    ),

    [user, registered_user].

get(UserId) ->
    F = fun() -> mnesia:read(user, UserId) end,
    [U] = mnesia:activity(transaction, F),
    U.

active_users() ->
    F = fun() -> mnesia:foldl(fun(User, Acc) -> [User | Acc] end, [], user) end,
    mnesia:activity(transaction, F).

registered_users() ->
    F = fun() -> mnesia:foldl(fun(User, Acc) -> [User | Acc] end, [], registered_user) end,
    mnesia:activity(transaction, F).

users_in_channel(ChannelId) ->
    Match = ets:fun2ms(fun(X = #user{channel_id = C}) when ChannelId =:= C -> X end),
    F = fun() -> mnesia:select(user, Match) end,
    mnesia:activity(transaction, F).

remove(UserId, SessionId, Actor, Reason, Ban) ->
    F = fun() -> mnesia:delete({user, UserId}) end,
    mnesia:activity(transaction, F).

unregister(UserId) ->
    F = fun() -> mnesia:delete({registered_user, UserId}) end,
    mnesia:activity(transaction, F).

add(Name) ->
    UserId = erlmur_id:new_user_id(),
    User =
        #user{
            name = Name,
            id = UserId,
            channel_id = erlmur_channel_store:default_channel_id()
        },
    F = fun() -> mnesia:write(User) end,
    mnesia:activity(transaction, F),
    logger:info("User added: ~p", [[{id, UserId}, {name, Name}]]),
    User.

update([], User, Actor) ->
    F = fun() -> mnesia:write(user, User, write) end,
    mnesia:activity(transaction, F);
update([{_, undefined} | R], User, Actor) ->
    update(R, User, Actor);
update([{channel_id, NewChannel} | R], User, Actor) ->
    update(R, User#user{channel_id = NewChannel}, Actor);
update([{name, NewName} | R], User, Actor) ->
    update(R, User#user{name = NewName}, Actor);
update([{comment, Comment} | R], User, Actor) ->
    update(R, User#user{comment = Comment}, Actor);
update([V | R], User, Actor) ->
    logger:warning("Unknown update field: ~p", [V]),
    update(R, User, Actor).

list() ->
    F = fun() -> mnesia:foldl(fun(User, Acc) -> [User | Acc] end, [], user) end,
    mnesia:activity(transaction, F).

move_to_channel([], _, _) ->
    ok;
move_to_channel([UserId | Users], ChannelId, Actor) ->
    move_to_channel(UserId, ChannelId, Actor),
    move_to_channel(Users, ChannelId, Actor);
move_to_channel(UserId, ChannelId, Actor) ->
    logger:info("Moving user ~p to channel ~p", [UserId, ChannelId]),
    F = fun() ->
        [U] = mnesia:read(user, UserId),
        NewU = U#user{channel_id = ChannelId},
        mnesia:write(NewU)
    end,
    mnesia:activity(transaction, F).

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
