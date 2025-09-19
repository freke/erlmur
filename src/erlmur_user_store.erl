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
    case mnesia:activity(transaction, F) of
        [U] -> {ok, U};
        [] -> {error, not_found}
    end.

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

update(UserId, Updates, _Actor) ->
    F = fun() ->
        case mnesia:read(user, UserId) of
            [OldUser] ->
                NewUser = apply_user_updates(OldUser, Updates),
                ok = mnesia:write(NewUser),
                {ok, NewUser};
            [] ->
                {error, user_not_found}
        end
    end,
    mnesia:activity(transaction, F).

list() ->
    F = fun() -> mnesia:foldl(fun(User, Acc) -> [User | Acc] end, [], user) end,
    mnesia:activity(transaction, F).

move_to_channel(UserIds, ChannelId, _Actor) when is_list(UserIds) ->
    logger:info("Moving users ~p to channel ~p", [UserIds, ChannelId]),
    F = fun() ->
        lists:foreach(
            fun(UserId) ->
                do_move_user(UserId, ChannelId)
            end,
            UserIds
        )
    end,
    mnesia:activity(transaction, F);
move_to_channel(UserId, ChannelId, Actor) when is_integer(UserId) ->
    move_to_channel([UserId], ChannelId, Actor).

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

do_move_user(UserId, NewChannelId) ->
    case mnesia:read(user, UserId) of
        [OldUser] ->
            NewUser = OldUser#user{channel_id = NewChannelId},
            mnesia:write(NewUser);
        [] ->
            ok
    end.

apply_user_updates(User, []) ->
    User;
apply_user_updates(User, [{_, undefined} | Rest]) ->
    apply_user_updates(User, Rest);
apply_user_updates(User, [{channel_id, V} | Rest]) ->
    apply_user_updates(User#user{channel_id = V}, Rest);
apply_user_updates(User, [{name, V} | Rest]) ->
    apply_user_updates(User#user{name = V}, Rest);
apply_user_updates(User, [{comment, V} | Rest]) ->
    apply_user_updates(User#user{comment = V}, Rest);
apply_user_updates(User, [{texture, V} | Rest]) ->
    apply_user_updates(User#user{texture = V}, Rest);
apply_user_updates(User, [Unknown | Rest]) ->
    logger:warning("Unknown update field: ~p", [Unknown]),
    apply_user_updates(User, Rest).
