-module(erlmur_channel_tests).

%-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).


erlmur_hannels_test_() ->
  [
    {"New channel",?setup(fun  new_channel_test/1)},
    {"Parent channel",?setup(fun  parent_channel_test/1)},
    {"Link channel",?setup(fun  link_channel_test/1)}
  ].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
  ok.

stop(_) ->
  ok.


%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
new_channel_test(_) ->
  Ch1 = erlmur_channel:new("Ch1"),
  Ch2 = erlmur_channel:new("Ch2"),
  [
    ?_assertEqual("Ch1", erlmur_channel:name(Ch1)),
    ?_assertEqual("Ch2", erlmur_channel:name(Ch2)),
    ?_assertNotEqual(erlmur_channel:channel_id(Ch1), erlmur_channel:channel_id(Ch2))
  ].

parent_channel_test(_) ->
  Ch1 = erlmur_channel:new("Ch1"),
  Ch2 = erlmur_channel:new("Ch2"),
  Ch3 = erlmur_channel:parent(erlmur_channel:channel_id(Ch1), Ch2),
  [
    ?_assertEqual(undefined, erlmur_channel:parent(Ch1)),
    ?_assertEqual(erlmur_channel:channel_id(Ch1), erlmur_channel:parent(Ch3))
  ].

link_channel_test(_) ->
  Ch1 = erlmur_channel:new("Ch1"),
  Ch2 = erlmur_channel:new("Ch2"),
  Ch3 = erlmur_channel:new("Ch3"),
  Ch4 = erlmur_channel:link(erlmur_channel:channel_id(Ch3), Ch2),
  Ch5 = erlmur_channel:link(erlmur_channel:channel_id(Ch3), Ch4),
  Ch6 = erlmur_channel:link(erlmur_channel:channel_id(Ch1), Ch5),
  Ch7 = erlmur_channel:unlink(erlmur_channel:channel_id(Ch3), Ch6),

  [
    ?_assertEqual(erlmur_channel:channel_id(Ch2), erlmur_channel:channel_id(Ch4)),
    ?_assertEqual(1, length(erlmur_channel:linked(Ch4))),
    ?_assertEqual([erlmur_channel:channel_id(Ch3)], erlmur_channel:linked(Ch4)),
    ?_assertEqual(1, length(erlmur_channel:linked(Ch5))),
    ?_assertEqual([erlmur_channel:channel_id(Ch3)], erlmur_channel:linked(Ch5)),
    ?_assertEqual(2, length(erlmur_channel:linked(Ch6))),
    ?_assertEqual(1, length(erlmur_channel:linked(Ch7))),
    ?_assertEqual([erlmur_channel:channel_id(Ch1)], erlmur_channel:linked(Ch7))
  ].

%%%%%%%%%%%%%%%%%%%%%%
%%% HELP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%
