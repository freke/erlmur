-module(erlmur_channels_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmur.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

erlmur_channels_test_() ->
    logger:set_primary_config(level, warning),
    [
        {"Creates root channel on init", ?setup(fun init_test/1)},
        {"Can add and find channels by name and ID", ?setup(fun find_test/1)},
        {"Links are symmetric after update", ?setup(fun link_test/1)},
        {"Remove deletes channel and back-links", ?setup(fun remove_test/1)},
        {"Filter selects matching channels", ?setup(fun filter_test/1)},
        {"Unknown link target is ignored", ?setup(fun unknown_target_test/1)}
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
    meck:reset(erlmur_channel_feed),
    meck:unload(erlmur_channel_feed).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
init_test(_) ->
    [Root] = erlmur_channels:list(),
    [
        ?_assertEqual("Root", Root#channel.name),
        ?_assertEqual(0, Root#channel.id),
        ?_assertEqual(undefined, Root#channel.parent_id)
    ].

find_test(_) ->
    meck:expect(erlmur_channel_feed, notify, fun(_) -> ok end),
    Ch = erlmur_channels:add(#{name => <<"RoomA">>, parent_id => 0}),
    [
        ?_assertMatch([_], erlmur_channels:find({name, <<"RoomA">>})),
        ?_assertEqual(Ch, erlmur_channels:fetch({id, Ch#channel.id})),
        ?_assert(meck:validate(erlmur_channel_feed))
    ].

link_test(_) ->
    meck:expect(erlmur_channel_feed, notify, fun(_) -> ok end),
    A = erlmur_channels:add(#{name => <<"A">>, parent_id => 0}),
    B = erlmur_channels:add(#{name => <<"B">>, parent_id => 0}),
    erlmur_channels:update(A#channel.id, #{links_add => [B#channel.id]}),

    A1 = erlmur_channels:fetch({id, A#channel.id}),
    B1 = erlmur_channels:fetch({id, B#channel.id}),
    [
        ?_assert(sets:is_element(B#channel.id, A1#channel.links)),
        ?_assert(sets:is_element(A#channel.id, B1#channel.links))
    ].

remove_test(_) ->
    meck:expect(erlmur_channel_feed, notify, fun(_) -> ok end),
    A = erlmur_channels:add(#{name => <<"A">>, parent_id => 0}),
    B = erlmur_channels:add(#{name => <<"B">>, parent_id => 0}),
    erlmur_channels:update(A#channel.id, #{links_add => [B#channel.id]}),
    ok = erlmur_channels:remove(B#channel.id, test),

    A1 = erlmur_channels:fetch({id, A#channel.id}),
    [
        ?_assertNot(sets:is_element(B#channel.id, A1#channel.links)),
        ?_assert(meck:validate(erlmur_channel_feed))
    ].

filter_test(_) ->
    meck:expect(erlmur_channel_feed, notify, fun(_) -> ok end),
    erlmur_channels:add(#{name => <<"X">>, parent_id => 0}),
    erlmur_channels:add(#{name => <<"Y">>, parent_id => 0}),
    Matches = erlmur_channels:filter(fun
        (#channel{name = <<"X">>}) -> true;
        (_) -> false
    end),
    [
        ?_assertMatch([#channel{name = <<"X">>}], Matches)
    ].

unknown_target_test(_) ->
    meck:expect(erlmur_channel_feed, notify, fun(_) -> ok end),
    A = erlmur_channels:add(#{name => <<"A">>, parent_id => 0}),
    % Nonexistent ID
    Unknown = 9999,

    Ch1 = erlmur_channels:update(A#channel.id, #{links_add => [Unknown]}),

    [
        ?_assertNot(sets:is_element(Unknown, Ch1#channel.links)),
        ?_assert(meck:validate(erlmur_channel_feed))
    ].

% -module(erlmur_channels_tests).
% 
% %-include_lib("proper/include/proper.hrl").
% -include_lib("eunit/include/eunit.hrl").
% 
% -include("erlmur.hrl").
% 
% -define(setup(F), {setup, fun start/0, fun stop/1, F}).
% 
% erlmur_hannels_test_() ->
%     logger:set_primary_config(level, warning),
%     [
%         {"Default channels values then started", ?setup(fun init_test/1)},
%         {"Find channel", ?setup(fun find_test/1)},
%         {"Link channels", ?setup(fun link_test/1)}
%     ].
% 
% %%%%%%%%%%%%%%%%%%%%%%%
% %%% SETUP FUNCTIONS %%%
% %%%%%%%%%%%%%%%%%%%%%%%
% start() ->
%     meck:new(erlmur_channel_feed),
%     application:start(mnesia),
%     erlmur_channels:init([node()]).
% 
% stop(_) ->
%     application:stop(mnesia),
%     meck:unload(erlmur_channel_feed).
% 
% %%%%%%%%%%%%%%%%%%%%
% %%% ACTUAL TESTS %%%
% %%%%%%%%%%%%%%%%%%%%
% init_test(_) ->
%     [RootChannel] = erlmur_channels:list(),
%     [
%         ?_assertEqual("Root", RootChannel#channel.name),
%         ?_assertEqual(0, RootChannel#channel.id),
%         ?_assertEqual(undefined, RootChannel#channel.parent)
%     ].
% 
% find_test(_) ->
%     meck:expect(erlmur_channel_feed, notify, fun(_) -> ok end),
%     Ch1 = erlmur_channels:add(#{name => "Ch1", parent_id => 0}),
%     [
%         ?_assertEqual([erlmur_channels:fetch({id, 0})], erlmur_channels:find({name, "Root"})),
%         ?_assertEqual([Ch1], erlmur_channels:find({name, "Ch1"})),
%         ?_assertEqual(Ch1, erlmur_channels:fetch({id, Ch1#channel.id}))
%     ].
% 
% link_test(_) ->
%     meck:expect(erlmur_channel_feed, notify, fun(_) -> ok end),
%     Ch1 = erlmur_channels:add(#{name => "Ch1", parent_id => 0}),
%     Ch2 = erlmur_channels:add(#{name => "Ch2", parent_id => 0}),
%     erlmur_channels:update(Ch1#channel.id, #{links_add => [Ch2#channel.id], links_remove => []}),
%     [
%         ?_assert(
%             sets:is_element(
%                 Ch2#channel.id,
%                 (erlmur_channels:fetch({id, Ch1#channel.id}))#channel.links
%             )
%         ),
%         ?_assert(
%             sets:is_element(
%                 Ch1#channel.id,
%                 (erlmur_channels:fetch({id, Ch2#channel.id}))#channel.links
%             )
%         ),
%         ?_assert(meck:validate(erlmur_channel_feed))
%     ].
% 
% %%%%%%%%%%%%%%%%%%%%%%
% %%% HELP FUNCTIONS %%%
% %%%%%%%%%%%%%%%%%%%%%%
