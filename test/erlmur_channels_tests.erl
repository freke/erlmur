-module(erlmur_channels_tests).

%-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).


erlmur_hannels_test_() ->
  [
    {"Default channels values then started",?setup(fun  init_test/1)},
    {"Find channel",?setup(fun  find_test/1)},
    {"Link channels",?setup(fun  link_test/1)}
  ].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
  meck:new(erlmur_channel_feed),
  application:start(mnesia),
  erlmur_channels:init([node()]).

stop(_) ->
  application:stop(mnesia),
  meck:unload(erlmur_channel_feed).


%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
init_test(_) ->
  [RootChannel] = erlmur_channels:channels(),
  [
    ?_assertEqual("Root", erlmur_channel:name(RootChannel)),
    ?_assertEqual(0, erlmur_channel:channel_id(RootChannel)),
    ?_assertEqual(undefined, erlmur_channel:parent(RootChannel))
  ].

find_test(_) ->
  Ch1 = erlmur_channel:parent(0, erlmur_channel:new("Ch1")),
  erlmur_channels:store(Ch1),
  [
    ?_assertEqual(erlmur_channels:find({channel_id, 0}), erlmur_channels:find({name, "Root"})),
    ?_assertEqual([Ch1], erlmur_channels:find({channel_id, erlmur_channel:channel_id(Ch1)})),
    ?_assertEqual([Ch1], erlmur_channels:find({name, "Ch1"}))
  ].

link_test(_) ->
  meck:expect(erlmur_channel_feed, notify, fun(_) -> ok end),
  Ch1 = erlmur_channel:parent(0, erlmur_channel:new("Ch1")),
  Ch2 = erlmur_channel:parent(0, erlmur_channel:new("Ch2")),
  erlmur_channels:store(Ch1),
  erlmur_channels:store(Ch2),
  erlmur_channels:channelstate([{channel_id,erlmur_channel:channel_id(Ch1)},{links_add,[erlmur_channel:channel_id(Ch2)]}]),
  [
    ?_assertEqual([erlmur_channel:channel_id(Ch2)], erlmur_channel:linked(hd(erlmur_channels:find({name, "Ch1"})))),
    ?_assertEqual([erlmur_channel:channel_id(Ch1)], erlmur_channel:linked(hd(erlmur_channels:find({name, "Ch2"})))),
    ?_assert(meck:validate(erlmur_channel_feed))
  ].

%%%%%%%%%%%%%%%%%%%%%%
%%% HELP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%
