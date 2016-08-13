-module(erlmur_channels_tests).

%-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).


erlmur_hannels_test_() ->
  [
    {"Default channels values then started",?setup(fun  init_test/1)},
    {"Find channel",?setup(fun  find_test/1)}
  ].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
  application:start(mnesia),
  erlmur_channels:init([node()]).

stop(_) ->
  ets:delete(channel_counters),
  application:stop(mnesia).


%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
init_test(_) ->
  [{channelstate, RootChannel}] = erlmur_channels:channelstates(),
  Name = proplists:get_value(name, RootChannel),
  Id = proplists:get_value(channel_id, RootChannel),
  Parent = proplists:get_value(parent, RootChannel),
  [
    ?_assertEqual("Root", Name),
    ?_assertEqual(0, Id),
    ?_assertEqual(undefined, Parent)
  ].

find_test(_) ->
  Channel1 = erlmur_channels:find({channel_id,0}),
  Channel2 = erlmur_channels:find({name,"Root"}),
  [
    ?_assertEqual(Channel1, Channel2)
  ].
%%%%%%%%%%%%%%%%%%%%%%
%%% HELP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%
