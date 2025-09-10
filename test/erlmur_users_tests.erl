%%%-------------------------------------------------------------------
%%% @author  David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013,
%%% @doc
%%%
%%% @end
%%% Created :  4 Apr 2013 by  <davabe@hotmail.com>
%%%-------------------------------------------------------------------
-module(erlmur_users_tests).

-include_lib("erlmur.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

erlmur_user_test_() ->
    logger:set_primary_config(level, warning),
    [
        {"Default user values then started", ?setup(fun init/1)},
        {"Select user by id", ?setup(fun select_by_id/1)},
        {"Add one user", ?setup(fun add_user/1)},
        {"Move user to channel", ?setup(fun move_user_to_channel/1)},
        {"Remove user", ?setup(fun remove_user/1)},
        {"List all users", ?setup(fun list_users/1)}
    ].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    meck:new(erlmur_channel_feed),
    meck:expect(erlmur_user_feed, notify, fun(_) -> ok end),
    application:start(mnesia),
    erlmur_id:start(),
    erlmur_users:init([node()]).

stop(_) ->
    erlmur_id:stop(),
    application:stop(mnesia),
    meck:unload(erlmur_channel_feed).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
init(_) ->
    [
        ?_assertEqual(0, length(erlmur_users:list())),
        ?_assertEqual([], erlmur_users:find({channel_id, 0}))
    ].

add_user(_) ->
    erlmur_users:add(<<"user1">>),
    [
        ?_assertEqual(1, length(erlmur_users:list())),
        ?_assertEqual(1, length(erlmur_users:find({channel_id, 0}))),
        ?_assertEqual(0, length(erlmur_users:find({channel_id, 1}))),
        ?_assert(meck:validate(erlmur_user_feed))
    ].

select_by_id(_) ->
    U1 = erlmur_users:add(<<"user1">>),
    U2 = erlmur_users:add(<<"user2">>),
    [
        ?_assertEqual(
            <<"user1">>,
            (erlmur_users:fetch({id, U1#user.id}))#user.name
        ),
        ?_assertEqual(
            <<"user2">>,
            (erlmur_users:fetch({id, U2#user.id}))#user.name
        ),
        ?_assert(meck:validate(erlmur_user_feed))
    ].

move_user_to_channel(_) ->
    U = erlmur_users:add(<<"user1">>),
    erlmur_users:move_to_channel(U#user.id, 1, U#user.id),
    [
        ?_assertEqual(1, length(erlmur_users:list())),
        ?_assertEqual(0, length(erlmur_users:find({channel_id, 0}))),
        ?_assertEqual(1, length(erlmur_users:find({channel_id, 1}))),
        ?_assert(meck:validate(erlmur_user_feed))
    ].

remove_user(_) ->
    U1 = erlmur_users:add(<<"user1">>),
    U2 = erlmur_users:add(<<"user2">>),
    erlmur_users:remove(U1#user.id, undefined, U2#user.id, "Test", false),
    [
        ?_assertEqual(1, length(erlmur_users:list())),
        ?_assertEqual(1, length(erlmur_users:find({channel_id, 0}))),
        ?_assert(meck:validate(erlmur_user_feed))
    ].

list_users(_) ->
    U1 = erlmur_users:add(<<"user1">>),
    U2 = erlmur_users:add(<<"user2">>),
    [
        ?_assertEqual(2, length(erlmur_users:list())),
        ?_assertEqual(
            true,
            lists:all(fun(E) -> E =:= U1 orelse E =:= U2 end, erlmur_users:list())
        ),
        ?_assert(meck:validate(erlmur_user_feed))
    ].

%%%%%%%%%%%%%%%%%%%%%%
%%% HELP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%
