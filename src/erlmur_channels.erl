-module(erlmur_channels).

-export(
	[
		init/1,
		store/1,
		find/1,
		channels/0,
    channelstate/1,
    channelstates/0,
    filter/1,
    remove/1
  ]).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("record_info/include/record_info.hrl").

-include("erlmur_channel.hrl").

-export_record_info([channel]).

-record(counter_entry, {id, value=0}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
init(Nodes) ->
  mnesia:create_table(channel,
		[
      {attributes, record_info(fields, channel)},
		  {ram_copies, Nodes},
			{index,[#channel.parent]},
		  {type, set}
    ]),

	ok = mnesia:wait_for_tables([channel],5000),
	save(erlmur_channel:new(root),[]).

store(Channel) ->
	F = fun() ->
		mnesia:write(channel, Channel, write)
	end,
  mnesia:activity(transaction, F).

find({channel_id, ChannelId}) ->
  Match = ets:fun2ms(fun(X = #channel{channel_id=ID}) when ChannelId =:= ID -> X end),
  F = fun() ->
		mnesia:select(channel, Match)
	end,
  mnesia:activity(transaction, F);
find({name, Name}) ->
  Match = ets:fun2ms(fun(X = #channel{name=N}) when Name =:= N -> X end),
  F = fun() ->
		mnesia:select(channel, Match)
	end,
  mnesia:activity(transaction, F).

channels() ->
	F = fun() ->
		mnesia:dirty_match_object(#channel{_ = '_'})
	end,
  mnesia:activity(transaction, F).

channelstates() ->
  F = fun() ->
		mnesia:foldl(fun(C, Acc) ->
        [{channelstate,[{permissions,16#f07ff}|record_info:record_to_proplist(C, ?MODULE)]} | Acc]
		  end,
			[],
			channel)
	end,
  mnesia:activity(transaction, F).

channelstate(PropList) ->
	Channel = save(find({channel_id, proplists:get_value(channel_id,PropList)}),PropList),
	erlmur_channel_feed:notify({update, lists:keyreplace(channel_id,1,PropList,{channel_id,Channel#channel.channel_id})}).

remove([]) ->
	ok;
remove([Channel|Rest]) ->
	F = fun() ->
		mnesia:delete({channel,Channel#channel.channel_id})
	end,
  mnesia:activity(transaction, F),
	erlmur_channel_feed:notify({removed,[{channel_id,Channel#channel.channel_id}]}),
	remove(Rest).

filter(Filter) ->
  F = fun() ->
		mnesia:foldl(fun(C, Acc) ->
        case Filter(C) of
            true ->
          [C|Acc];
            false ->
          Acc
        end
		  end,
			[],
			channel)
	end,
  mnesia:activity(transaction, F).

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
save([], PropList) ->
	Channel = erlmur_channel:new(proplists:get_value(name, PropList)),
	save(Channel, PropList);
save([Channel], PropList) ->
	save(Channel, PropList);
save(Channel, []) ->
	store(Channel),
	Channel;
save(Channel, [{parent,Parent}|Rest]) ->
	save(erlmur_channel:parent(Parent, Channel),Rest);
save(Channel, [{links_add,AddLinks}|Rest]) ->
	save(lists:foldl(
		fun(ChannelId, Channel) ->
			[C2] = find({channel_id, ChannelId}),
			store(erlmur_channel:link(ChannelId, Channel)),
			erlmur_channel:link(erlmur_channel:channel_id(Channel), C2)
		end,
		Channel,
		AddLinks
		), Rest);
save(Channel, [{links_remove,DelLinks}|Rest]) ->
	save(Channel#channel{links=lists:subtract(Channel#channel.links, DelLinks)}, Rest);
save(Channel, [_|Rest]) ->
	save(Channel,Rest).
