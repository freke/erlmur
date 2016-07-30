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
		subchannels/1
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
	[channel].

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

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
add(Name) ->
  ChannelId = ets:update_counter(channel_counters, channelid, {#counter_entry.value, 1}),
  #channel{
    name=Name,
    channel_id=ChannelId
  }.

channelstate(PropList) ->
	Channel = channelstate(find({channel_id, proplists:get_value(channel_id,PropList)}),PropList),
	erlmur_channel_feed:notify({update, lists:keyreplace(channel_id,1,PropList,{channel_id,Channel#channel.channel_id})}).

channelstate([], PropList) ->
	Channel = add(proplists:get_value(name, PropList)),
	channelstate(Channel, PropList);
channelstate([Channel], PropList) ->
	channelstate(Channel, PropList);
channelstate(Channel, []) ->
	F = fun() ->
		mnesia:write(channel, Channel, write)
	end,
  mnesia:activity(transaction, F),
	Channel;
channelstate(Channel, [{parent,Parent}|Rest]) ->
	channelstate(Channel#channel{parent=Parent},Rest);
channelstate(Channel, [{links_add,AddLinks}|Rest]) ->
	channelstate(Channel#channel{links=lists:umerge(lists:sort(AddLinks) , Channel#channel.links)}, Rest);
channelstate(Channel, [{links_remove,DelLinks}|Rest]) ->
	channelstate(Channel#channel{links=lists:subtract(Channel#channel.links, DelLinks)}, Rest);
channelstate(Channel, [_|Rest]) ->
	channelstate(Channel,Rest).

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

subchannels(_Channel) ->
	[].

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
