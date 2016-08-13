%%%-------------------------------------------------------------------
%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013,
%%% @doc
%%%
%%% @end
%%% Created :  1 Apr 2013 by David AAberg <davabe@hotmail.com>
%%%-------------------------------------------------------------------
-module(erlmur_channels).

-export(
	[
		init/1,
    channelstate/1,
    find/1,
    channelstates/0,
    filter/1,
    remove/1,
    name/1,
    channel_id/1,
		linked/1
  ]).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("record_info/include/record_info.hrl").

-export_record_info([channel]).

-record(channel,{channel_id,parent,name,links=[]}).
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

  ets:new(channel_counters, [set, {keypos, #counter_entry.id}, named_table, public]),
  ets:insert(channel_counters, #counter_entry{id=channelid, value=-1}),

	ok = mnesia:wait_for_tables([channel],5000),
	add(new("Root"),[]).

name(Channel) when is_record(Channel,channel) ->
  Channel#channel.name.

channel_id(Channel) when is_record(Channel,channel) ->
  Channel#channel.channel_id.

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
	Channel = add(find({channel_id, proplists:get_value(channel_id,PropList)}),PropList),
	erlmur_channel_feed:notify({update, lists:keyreplace(channel_id,1,PropList,{channel_id,Channel#channel.channel_id})}).

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

linked(Channel) ->
	[].

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
new(Name) ->
  ChannelId = ets:update_counter(channel_counters, channelid, {#counter_entry.value, 1}),
  #channel{
    name=Name,
    channel_id=ChannelId
  }.

add([], PropList) ->
	Channel = new(proplists:get_value(name, PropList)),
	add(Channel, PropList);
add([Channel], PropList) ->
	add(Channel, PropList);
add(Channel, []) ->
	F = fun() ->
		mnesia:write(channel, Channel, write)
	end,
  mnesia:activity(transaction, F),
	Channel;
add(Channel, [{parent,Parent}|Rest]) ->
	add(Channel#channel{parent=Parent},Rest);
add(Channel, [{links_add,AddLinks}|Rest]) ->
	add(Channel#channel{links=lists:umerge(lists:sort(AddLinks) , Channel#channel.links)}, Rest);
add(Channel, [{links_remove,DelLinks}|Rest]) ->
	add(Channel#channel{links=lists:subtract(Channel#channel.links, DelLinks)}, Rest);
add(Channel, [_|Rest]) ->
	add(Channel,Rest).
