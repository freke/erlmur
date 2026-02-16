-module(erlmur_channel_manager_tests).

-moduledoc """
EUnit tests for erlmur_channel_manager.

These tests verify channel creation, removal, updates, and broadcasting
functionality.
""".

-include_lib("eunit/include/eunit.hrl").
-include("erlmur.hrl").

%%%===================================================================
%%% Test Setup/Teardown
%%%===================================================================

setup() ->
    {ok, Pid} = erlmur_channel_manager:start_link(),
    Pid.

teardown(Pid) ->
    gen_server:stop(Pid),
    ok.

%%%===================================================================
%%% Test Generators
%%%===================================================================

channel_manager_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun test_ensure_root_channel/0,
      fun test_create_channel/0,
      fun test_create_channel_with_parent/0,
      fun test_create_channel_parent_not_found/0,
      fun test_create_channel_auto_id/0,
      fun test_remove_channel/0,
      fun test_remove_nonexistent_channel/0,
      fun test_remove_root_channel/0,
      fun test_get_channel/0,
      fun test_get_all_channels/0,
      fun test_update_channel/0,
      fun test_update_nonexistent_channel/0,
      fun test_channel_hierarchy/0
     ]}.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_ensure_root_channel() ->
    [?_test(
        begin
            ok = erlmur_channel_manager:ensure_root_channel(),
            {ok, Channel} = erlmur_channel_manager:get_channel(0),
            ?assertEqual(0, Channel#channel.id),
            ?assertEqual(<<"Root">>, Channel#channel.name),
            ?assertEqual(undefined, Channel#channel.parent_id)
        end
    )].

test_create_channel() ->
    [?_test(
        begin
            ok = erlmur_channel_manager:ensure_root_channel(),
            ChannelState = #{
                name => <<"Test Channel">>,
                parent => 0,
                description => <<"A test channel">>,
                position => 1
            },
            ok = erlmur_channel_manager:create_channel(ChannelState),
            {ok, Channel} = erlmur_channel_manager:get_channel(1),
            ?assertEqual(<<"Test Channel">>, Channel#channel.name),
            ?assertEqual(0, Channel#channel.parent_id),
            ?assertEqual(<<"A test channel">>, Channel#channel.description),
            ?assertEqual(1, Channel#channel.position)
        end
    )].

test_create_channel_with_parent() ->
    [?_test(
        begin
            ok = erlmur_channel_manager:ensure_root_channel(),
            ParentState = #{
                name => <<"Parent">>,
                parent => 0
            },
            ok = erlmur_channel_manager:create_channel(ParentState),
            ChildState = #{
                name => <<"Child">>,
                parent => 1
            },
            ok = erlmur_channel_manager:create_channel(ChildState),
            {ok, Child} = erlmur_channel_manager:get_channel(2),
            ?assertEqual(<<"Child">>, Child#channel.name),
            ?assertEqual(1, Child#channel.parent_id)
        end
    )].

test_create_channel_parent_not_found() ->
    [?_test(
        begin
            ok = erlmur_channel_manager:ensure_root_channel(),
            ChannelState = #{
                name => <<"Orphan Channel">>,
                parent => 999
            },
            ok = erlmur_channel_manager:create_channel(ChannelState),
            ?assertEqual({error, not_found}, 
                        erlmur_channel_manager:get_channel(1))
        end
    )].

test_create_channel_auto_id() ->
    [?_test(
        begin
            ok = erlmur_channel_manager:ensure_root_channel(),
            ok = erlmur_channel_manager:create_channel(#{
                name => <<"Channel 1">>,
                parent => 0
            }),
            ok = erlmur_channel_manager:create_channel(#{
                name => <<"Channel 2">>,
                parent => 0
            }),
            ok = erlmur_channel_manager:create_channel(#{
                name => <<"Channel 3">>,
                parent => 0
            }),
            {ok, C1} = erlmur_channel_manager:get_channel(1),
            {ok, C2} = erlmur_channel_manager:get_channel(2),
            {ok, C3} = erlmur_channel_manager:get_channel(3),
            ?assertEqual(<<"Channel 1">>, C1#channel.name),
            ?assertEqual(<<"Channel 2">>, C2#channel.name),
            ?assertEqual(<<"Channel 3">>, C3#channel.name)
        end
    )].

test_remove_channel() ->
    [?_test(
        begin
            ok = erlmur_channel_manager:ensure_root_channel(),
            ChannelState = #{
                name => <<"To Be Removed">>,
                parent => 0
            },
            ok = erlmur_channel_manager:create_channel(ChannelState),
            {ok, _} = erlmur_channel_manager:get_channel(1),
            ok = erlmur_channel_manager:remove_channel(1),
            ?assertEqual({error, not_found}, 
                        erlmur_channel_manager:get_channel(1))
        end
    )].

test_remove_nonexistent_channel() ->
    [?_test(
        begin
            ok = erlmur_channel_manager:ensure_root_channel(),
            ok = erlmur_channel_manager:remove_channel(999)
        end
    )].

test_remove_root_channel() ->
    [?_test(
        begin
            ok = erlmur_channel_manager:ensure_root_channel(),
            ok = erlmur_channel_manager:remove_channel(0),
            {ok, _} = erlmur_channel_manager:get_channel(0)
        end
    )].

test_get_channel() ->
    [?_test(
        begin
            ok = erlmur_channel_manager:ensure_root_channel(),
            ChannelState = #{
                name => <<"Test">>,
                parent => 0
            },
            ok = erlmur_channel_manager:create_channel(ChannelState),
            {ok, Channel} = erlmur_channel_manager:get_channel(1),
            ?assertEqual(1, Channel#channel.id),
            ?assertEqual(<<"Test">>, Channel#channel.name)
        end
    )].

test_get_all_channels() ->
    [?_test(
        begin
            ok = erlmur_channel_manager:ensure_root_channel(),
            ok = erlmur_channel_manager:create_channel(#{name => <<"Ch1">>, parent => 0}),
            ok = erlmur_channel_manager:create_channel(#{name => <<"Ch2">>, parent => 0}),
            Channels = erlmur_channel_manager:get_all_channels(),
            ?assertEqual(3, length(Channels)),
            ChannelIds = lists:sort([C#channel.id || C <- Channels]),
            ?assertEqual([0, 1, 2], ChannelIds)
        end
    )].

test_update_channel() ->
    [?_test(
        begin
            ok = erlmur_channel_manager:ensure_root_channel(),
            ok = erlmur_channel_manager:create_channel(#{
                name => <<"Original">>,
                parent => 0,
                description => <<"Original desc">>
            }),
            UpdateState = #{
                name => <<"Updated">>,
                description => <<"Updated desc">>
            },
            ok = erlmur_channel_manager:update_channel(1, UpdateState),
            {ok, Channel} = erlmur_channel_manager:get_channel(1),
            ?assertEqual(<<"Updated">>, Channel#channel.name),
            ?assertEqual(<<"Updated desc">>, Channel#channel.description)
        end
    )].

test_update_nonexistent_channel() ->
    [?_test(
        begin
            ok = erlmur_channel_manager:ensure_root_channel(),
            ok = erlmur_channel_manager:update_channel(999, #{name => <<"Test">>})
        end
    )].

test_channel_hierarchy() ->
    [?_test(
        begin
            ok = erlmur_channel_manager:ensure_root_channel(),
            ok = erlmur_channel_manager:create_channel(#{name => <<"Parent1">>, parent => 0}),
            ok = erlmur_channel_manager:create_channel(#{name => <<"Child1_1">>, parent => 1}),
            ok = erlmur_channel_manager:create_channel(#{name => <<"Child1_2">>, parent => 1}),
            ok = erlmur_channel_manager:create_channel(#{name => <<"GrandChild">>, parent => 2}),
            {ok, GrandChild} = erlmur_channel_manager:get_channel(4),
            ?assertEqual(3, GrandChild#channel.parent_id)
        end
    )].
