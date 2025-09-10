-module(erlmur_channel_store).

-moduledoc """
Manages hierarchical channels and their relationships.

This module handles the creation, modification, and deletion of channels,
including their links and ACLs, and interacts with Mnesia for persistence.
""".

-export([
    init/1,
    create_default/0,
    default_channel_id/0,
    add/1,
    fetch/1,
    find/1,
    update/2,
    list/0,
    filter/1,
    remove/2
]).

-include_lib("stdlib/include/ms_transform.hrl").

-include("erlmur.hrl").

-type channel() :: #channel{}.

-export_type([channel/0]).

-define(ROOT_CHANNEL_ID, 0).

-doc "Initializes the system and ensures the root channel exists.".

-spec init([node()]) -> [atom()].
init(Nodes) ->
    mnesia:create_table(
        channel,
        [
            {attributes, record_info(fields, channel)},
            {ram_copies, Nodes},
            {index, [#channel.parent_id]},
            {type, set}
        ]
    ),
    [channel].

create_default() ->
    F = fun() ->
        case mnesia:read(channel, ?ROOT_CHANNEL_ID) of
            [] -> mnesia:write(#channel{name = "Root", id = ?ROOT_CHANNEL_ID});
            _ -> ok
        end
    end,
    mnesia:activity(transaction, F).

-doc "The default entry channel.".
-spec default_channel_id() -> non_neg_integer().
default_channel_id() ->
    ?ROOT_CHANNEL_ID.

-doc "Creates a new channel using the provided properties and returns the complete channel.".
-spec add(map()) -> channel().
add(Updates) when is_map(Updates) ->
    ChannelId = erlang:unique_integer([monotonic, positive]),
    F = fun() ->
        Empty = #channel{id = ChannelId},
        Clean = maps:without([links_add, links_remove], Updates),
        Channel0 = merge_channel(Empty, Clean),
        logger:debug("Merge ~p~nand ~p~nresult ~p", [Empty, Clean, Channel0]),

        RequestedLinks =
            sets:from_list(
                maps:get(links_add, Updates, [])
            ),
        {ValidLinks, _} = sync_backlinks(ChannelId, sets:new(), RequestedLinks),

        FinalChannel = Channel0#channel{links = ValidLinks},
        logger:info("New Channel created ~p", [FinalChannel]),
        mnesia:write(FinalChannel),

        FinalChannel
    end,
    mnesia:activity(transaction, F).

-doc "Retrieves a channel by its unique ID.\nFails if the channel does not exist.".
-spec fetch({id, non_neg_integer()}) -> channel().
fetch({id, ChannelId}) ->
    F = fun() -> mnesia:read(channel, ChannelId) end,
    [C] = mnesia:activity(transaction, F),
    C.

-doc "Finds all channels matching a given property query.".
-spec find({name, binary()}) -> [channel()].
find({name, Name}) ->
    Match = ets:fun2ms(fun(X = #channel{name = N}) when Name =:= N -> X end),
    F = fun() -> mnesia:select(channel, Match) end,
    mnesia:activity(transaction, F).

-doc "Returns all known channels.".
-spec list() -> [channel()].
list() ->
    F = fun() -> mnesia:foldl(fun(C, Acc) -> [C | Acc] end, [], channel) end,
    mnesia:activity(transaction, F).

-doc "Applies partial updates to a channel's properties and link relationships.".
-spec update(non_neg_integer(), map()) -> channel().
update(ChannelId, Updates) when is_integer(ChannelId), is_map(Updates) ->
    F = fun() ->
        [Old] = mnesia:read(channel, ChannelId),
        OldLinks = Old#channel.links,

        Add = sets:from_list(
            maps:get(links_add, Updates, [])
        ),
        Remove =
            sets:from_list(
                maps:get(links_remove, Updates, [])
            ),

        DesiredLinks =
            sets:union(
                sets:subtract(OldLinks, Remove), Add
            ),
        {ValidAdd, ValidRemove} = sync_backlinks(ChannelId, OldLinks, DesiredLinks),

        NewLinks =
            sets:union(
                sets:subtract(OldLinks, ValidRemove), ValidAdd
            ),

        Clean = maps:without([links_add, links_remove], Updates),
        Channel0 = merge_channel(Old, Clean),
        FinalChannel = Channel0#channel{links = NewLinks},

        mnesia:write(FinalChannel),
        FinalChannel
    end,
    mnesia:activity(transaction, F).

-doc "Removes a channel by ID and notifies subscribers of its deletion.".
-spec remove(non_neg_integer(), term()) -> ok.
remove(ChannelId, _Actor) ->
    F = fun() ->
        case mnesia:read(channel, ChannelId) of
            [C] ->
                lists:foreach(
                    fun(RemoteId) -> remove_backlink(RemoteId, ChannelId) end,
                    sets:to_list(C#channel.links)
                ),
                mnesia:delete({channel, ChannelId});
            [] ->
                ok
        end
    end,
    mnesia:activity(transaction, F),
    ok.

-doc "Returns a list of channels matching a given filter function.".
-spec filter(fun((channel()) -> boolean())) -> [channel()].
filter(Filter) ->
    F = fun() ->
        mnesia:foldl(
            fun(C, Acc) ->
                case Filter(C) of
                    true -> [C | Acc];
                    false -> Acc
                end
            end,
            [],
            channel
        )
    end,
    mnesia:activity(transaction, F).

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

add_backlink(RemoteId, SelfId) when RemoteId =/= SelfId ->
    case mnesia:read(channel, RemoteId) of
        [Remote] ->
            NewRemote = Remote#channel{links = sets:add_element(SelfId, Remote#channel.links)},
            mnesia:write(NewRemote),
            {ok, RemoteId};
        [] ->
            {fail, RemoteId}
    end;
add_backlink(RemoteId, _) ->
    {fail, RemoteId}.

remove_backlink(RemoteId, SelfId) when RemoteId =/= SelfId ->
    case mnesia:read(channel, RemoteId) of
        [Remote] ->
            NewRemote = Remote#channel{links = sets:del_element(SelfId, Remote#channel.links)},
            mnesia:write(NewRemote),
            {ok, RemoteId};
        [] ->
            {fail, RemoteId}
    end;
remove_backlink(RemoteId, _) ->
    {fail, RemoteId}.

merge_channel(Record, Updates) when is_map(Updates) ->
    Fields = record_info(fields, channel),
    IgnoredKeys = [links_add, links_remove],
    ValidUpdateKeys = maps:keys(Updates) -- IgnoredKeys,
    UnknownKeys = ValidUpdateKeys -- Fields,
    case UnknownKeys of
        [] ->
            ok;
        _ ->
            logger:warning("Unknown channel fields: ~p", [UnknownKeys])
    end,
    lists:foldl(
        fun(Field, Rec) ->
            case maps:find(Field, Updates) of
                {ok, Value} ->
                    Index = field_index(Field, Fields),
                    % +1 to skip tag
                    setelement(Index + 1, Rec, Value);
                error ->
                    Rec
            end
        end,
        Record,
        Fields
    ).

field_index(Field, Fields) ->
    field_index(Field, Fields, 1).

field_index(Field, [Field | _T], Index) ->
    Index;
field_index(Field, [_H | T], Index) ->
    field_index(Field, T, Index + 1).

sync_backlinks(ChannelId, OldLinks, NewLinks) ->
    Added = sets:subtract(NewLinks, OldLinks),
    Removed = sets:subtract(OldLinks, NewLinks),

    EffectiveAdd =
        lists:filtermap(
            fun(RemoteId) ->
                case add_backlink(RemoteId, ChannelId) of
                    {ok, Id} -> {true, Id};
                    {fail, _} -> false
                end
            end,
            sets:to_list(Added)
        ),

    EffectiveRemove =
        lists:filtermap(
            fun(RemoteId) ->
                case remove_backlink(RemoteId, ChannelId) of
                    {ok, Id} -> {true, Id};
                    {fail, _} -> false
                end
            end,
            sets:to_list(Removed)
        ),

    {sets:from_list(EffectiveAdd), sets:from_list(EffectiveRemove)}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

merge_channel_simple_update_test() ->
    Channel = #channel{id = 1, name = <<"Original Name">>},
    Updates = #{name => <<"New Name">>, description => <<"A Description">>},
    Expected = Channel#channel{name = <<"New Name">>, description = <<"A Description">>},
    ?assertEqual(Expected, merge_channel(Channel, Updates)).

merge_channel_ignores_special_keys_test() ->
    Channel = #channel{id = 1, name = <<"Original Name">>},
    Updates = #{name => <<"New Name">>, links_add => [2], links_remove => [3]},
    Expected = Channel#channel{name = <<"New Name">>},
    ?assertEqual(Expected, merge_channel(Channel, Updates)).

merge_channel_ignores_unknown_keys_test() ->
    Channel = #channel{id = 1, name = <<"Original Name">>},
    Updates = #{name => <<"New Name">>, unknown_key => "some value"},
    Expected = Channel#channel{name = <<"New Name">>},
    ?assertEqual(Expected, merge_channel(Channel, Updates)).

merge_channel_empty_updates_test() ->
    Channel = #channel{id = 1, name = <<"Original Name">>},
    Updates = #{},
    ?assertEqual(Channel, merge_channel(Channel, Updates)).

-endif.
