%%%-------------------------------------------------------------------
%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 31 Mar 2013 by David AAberg <davabe@hotmail.com>
%%%-------------------------------------------------------------------
-module(erlmur_channels).

-export([init/0,
	 name/1,
	 id/1,
	 all_channel_states/0,
	 find_by_id/1,
	 find_by_name/1,
	 list/0,
	 add/1,
	 update/1,
	 remove/1]).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("record_info/include/record_info.hrl").

-record(channel,{channel_id,parent,name}).
-record(counter_entry, {id, value=0}).

-export_record_info([channel]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
init() ->
    ets:new(channels, [set, {keypos,#channel.channel_id},named_table, public]),
    ets:new(channel_counters, [set, {keypos, #counter_entry.id}, named_table, public]),
    ets:insert(channel_counters, #counter_entry{id=channelid, value=0}).

name(Channel) ->
    Channel#channel.name.

id(Channel) ->
    Channel#channel.channel_id.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
find_by_id(ChannelId) ->
    Match = ets:fun2ms(fun(X = #channel{channel_id=Id}) when Id =:= ChannelId -> X end),
    ets:select(channels, Match).

find_by_name(Name) ->
    Match = ets:fun2ms(fun(X = #channel{name=N}) when N =:= Name -> X end),
    ets:select(channels, Match).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
list() ->
    ets:foldl(fun(Channel,Acc) -> [Channel|Acc] end, [], channels).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
add(Channel) ->
    ChannelId = ets:update_counter(channel_counters, channelid, {#counter_entry.value, 1}),
    C = record_info:proplist_to_record(Channel, channel, ?MODULE),
    NewChannel = C#channel{channel_id=ChannelId},
    ets:insert(channels,NewChannel),
    erlmur_users:send_to_all(channelstate(NewChannel)),
    error_logger:info_report([{erlmur_channels,add},{new_channel,NewChannel}]),
    NewChannel.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
update(Channel) ->
    [C] = find_by_id(proplists:get_value(channel_id,Channel)),
    NewChannel = update(Channel,C),
    erlmur_users:send_to_all(channelstate(NewChannel)),
    error_logger:info_report([{erlmur_channels,update},{channel,NewChannel}]),
    NewChannel.

update([],C) ->
    C;
update([{parent,Parent}|Rest],C) ->
    update(Rest,C#channel{parent=Parent});
update([{name,Name}|Rest],C) ->
    update(Rest,C#channel{name=Name});
update([_|Rest],C) ->
    update(Rest,C).

    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
remove([]) ->
    ok;
remove([Channel|Channels]) ->
    Match = ets:fun2ms(fun(X = #channel{parent=Id}) when Id =:= Channel#channel.channel_id -> X end),
    SubChannels = ets:select(channels, Match),

    remove(SubChannels),

    lists:foreach(fun(U) -> erlmur_users:move_to_channel(U,Channel#channel.parent) end, 
		  erlmur_users:find_user({channel_id,Channel#channel.channel_id})),
    ets:delete(channels,Channel#channel.channel_id),
    erlmur_users:send_to_all(channelremove(Channel)),
    error_logger:info_report([{erlmur_channels,remove},{channel,Channel}]),
    remove(Channels).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
all_channel_states() ->
    ets:foldl(fun(Channel,Acc) -> [channelstate(Channel)|Acc] end, [], channels).

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
channelstate(Channel) ->
    erlmur_message:channelstate(record_info:record_to_proplist(Channel, ?MODULE)).

channelremove(Channel) ->
    erlmur_message:channelremove(record_info:record_to_proplist(Channel, ?MODULE)).
