%%%-------------------------------------------------------------------
%%% @author  David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created :  4 Apr 2013 by  <davabe@hotmail.com>
%%%-------------------------------------------------------------------
-module(erlmur_users_tests).

%-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).


erlmur_user_test_() ->
    [{"Default values then started",?setup(fun  init/1)},
     {"Select user by id",?setup(fun select_by_id/1)},
     {"Add one user",?setup(fun add_user/1)},
     {"Move user to channel",?setup(fun move_user_to_channel/1)},
     {"Remove user",?setup(fun remove_user/1)},
     {"Find user form Pid",?setup(fun find_user_from_pid/1)},
     {"List all users",?setup(fun list_users/1)}
    ].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    meck:new(erlmur_client),
    application:start(mnesia),
    erlmur_users:init([node()]).

stop(_) ->
    ets:delete(user_counters),
    application:stop(mnesia),
    meck:unload(erlmur_client).


%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
init(_) ->
    [?_assertEqual(0, erlmur_users:count()),
     ?_assertEqual([], erlmur_users:in_channel(0))].

add_user(_) ->
    erlmur_users:add(self(),user1,1),
    [?_assertEqual(1, erlmur_users:count()),
     ?_assertEqual(1, length(erlmur_users:in_channel(0))),
     ?_assertEqual(0, length(erlmur_users:in_channel(1))),
     ?_assert(meck:validate(erlmur_client))].

select_by_id(_) ->
    U1 = erlmur_users:add(self(),user1,28171),
    U2 = erlmur_users:add(self(),user2,29287),
    [?_assertEqual(user1, erlmur_users:name(erlmur_users:fetch_user({id,U1}))),
     ?_assertEqual(user2, erlmur_users:name(erlmur_users:fetch_user({id,U2}))),
     ?_assert(meck:validate(erlmur_client))].
    

move_user_to_channel(_) ->
    U = add_user(self(),user1,1),
    erlmur_users:move_to_channel(U,1),
    [?_assertEqual(1, erlmur_users:count()),
     ?_assertEqual(0, length(erlmur_users:in_channel(0))),
     ?_assertEqual(1, length(erlmur_users:in_channel(1))),
     ?_assert(meck:validate(erlmur_client))].

remove_user(_) ->
    U1 = add_user(self(),user1,1),
    U2 = add_user(self(),user2,2),
    erlmur_users:remove(U1,"Test"),
    [?_assertEqual(1, erlmur_users:count()),
     ?_assertEqual(1, length(erlmur_users:in_channel(0))),
     ?_assert(meck:validate(erlmur_client))].

find_user_from_pid(_) ->
    Pid = self(),
    U1 = add_user(Pid,user1,1),
    [?_assertEqual(U1, erlmur_users:fetch_user({client_pid,Pid})),
     ?_assert(meck:validate(erlmur_client))].

list_users(_) ->
    U1 = add_user(self(),user1,1),
    U2 = add_user(self(),user2,2),
    [?_assertEqual(2, erlmur_users:count()),
     ?_assertEqual(2, length(erlmur_users:list()))].

%%%%%%%%%%%%%%%%%%%%%%
%%% HELP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%
add_user(Pid,User,Address) ->
    erlmur_users:fetch_user({session,erlmur_users:add(Pid,User,Address)}).
