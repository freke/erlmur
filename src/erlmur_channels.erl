%%%-------------------------------------------------------------------
%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 31 Mar 2013 by David AAberg <davabe@hotmail.com>
%%%-------------------------------------------------------------------
-module(erlmur_channels).

-export([init/1,
	 name/1,
	 id/1,
	 all_channel_states/0,
	 find_by_id/1,
	 find_by_name/1,
	 list/0,
	 subchannels/1,
	 add/1,
	 add/2,
	 update/1,
	 remove/1,
	 permissions/2]).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("record_info/include/record_info.hrl").

-define(PERM_NONE, 16#0).
-define(PERM_WRITE, 16#1).
-define(PERM_TRAVERSE, 16#2).
-define(PERM_ENTER, 16#4).
-define(PERM_SPEAK, 16#8).
-define(PERM_MUTEDEAFEN, 16#10).
-define(PERM_MOVE, 16#20).
-define(PERM_MAKECHANNEL, 16#40).
-define(PERM_LINKCHANNEL, 16#80).
-define(PERM_WHISPER, 16#100).
-define(PERM_TEXTMESSAGE, 16#200).
-define(PERM_MAKETEMPCHANNEL, 16#400).
% Root channel only
-define(PERM_KICK, 16#10000).
-define(PERM_BAN, 16#20000).
-define(PERM_REGISTER, 16#40000).
-define(PERM_SELFREGISTER, 16#80000).
-define(PERM_CACHED, 16#8000000).
-define(PERM_ALL, 16#f07ff).

-record(channel,{channel_id,parent,name,permissions}).
-record(counter_entry, {id, value=0}).

-export_record_info([channel]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
init(Nodes) ->
    mnesia:create_table(channel,
			[{attributes, record_info(fields, channel)},
			 {ram_copies, Nodes},
			 {type, set}]),

    ets:new(channels, [set, {keypos,#channel.channel_id},named_table, public]),
    ets:new(channel_counters, [set, {keypos, #counter_entry.id}, named_table, public]),
    ets:insert(channel_counters, #counter_entry{id=channelid, value=0}),
    ok = mnesia:wait_for_tables([channel],5000).

name(Channel) ->
    Channel#channel.name.

id(Channel) ->
    Channel#channel.channel_id.

permissions(Channel,User) ->
    ?PERM_ALL. %?PERM_NONE.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
find_by_id([]) ->
    [];
find_by_id([Channel|Channels]) ->
    lists:append(find_by_id(Channel),find_by_id(Channels));
find_by_id(ChannelId) ->
    Match = ets:fun2ms(fun(X = #channel{channel_id=Id}) when Id =:= ChannelId -> X end),
    F = fun() ->
		mnesia:select(channel, Match)
	end,
    mnesia:activity(transaction, F).

find_by_name(Name) ->
    Match = ets:fun2ms(fun(X = #channel{name=N}) when N =:= Name -> X end),
    F = fun() ->
		mnesia:select(channel, Match)
	end,
    mnesia:activity(transaction, F).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
list() ->
    F = fun() ->
		mnesia:foldl(fun(Channel,Acc) -> [Channel|Acc] end, [], channel)
	end,
    mnesia:activity(transaction, F).

subchannels([]) ->
    [];
subchannels([Channel|Channels]) ->
    lists:append(subchannels(Channel),subchannels(Channels));
subchannels(Channel) ->
    subchannels(internal_subchannels(Channel),[]).

subchannels([],Result) ->
    lists:reverse(Result);
subchannels([Channel|Channels],Result) ->
    Sub = internal_subchannels(Channel),
    subchannels(lists:append(Channels,Sub),[Channel|Result]).

internal_subchannels(Channel) ->
    Match = ets:fun2ms(fun(X = #channel{parent=Id}) when Id =:= Channel#channel.channel_id -> X end),
    F = fun() ->
		mnesia:select(channel, Match)
	end,
    mnesia:activity(transaction, F).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
add(Channel) ->
    ChannelId = ets:update_counter(channel_counters, channelid, {#counter_entry.value, 1}),
    C = record_info:proplist_to_record(Channel, channel, ?MODULE),
    NewChannel = C#channel{channel_id=ChannelId},
    F = fun() ->
		mnesia:write(NewChannel)
	end,
    mnesia:activity(transaction, F),
    erlmur_users:send_to_all(channelstate(NewChannel)),
    error_logger:info_report([{erlmur_channels,add},{new_channel,NewChannel}]),
    NewChannel.

add(Id,Name) ->
    Channel = #channel{channel_id=Id,name=Name},
    F = fun() ->
		mnesia:write(Channel)
	end,
    mnesia:activity(transaction, F),
    erlmur_users:send_to_all(channelstate(Channel)),
    error_logger:info_report([{erlmur_channels,add},{new_channel,Channel}]),
    Channel.

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
    error_logger:info_report([{erlmur_channels,remove},{channel,Channel}]),
    SubChannels = subchannels(Channel),

    remove(SubChannels),
    
    lists:foreach(fun(U) -> erlmur_users:move_to_channel(U,Channel#channel.parent) end, 
		  erlmur_users:find_user({channel_id,Channel#channel.channel_id})),
    
    F = fun() ->
		mnesia:delete({channel,Channel#channel.channel_id})
	end,
    mnesia:activity(transaction, F),
    erlmur_users:send_to_all(channelremove(Channel)),
    remove(Channels).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
all_channel_states() ->
    [Root] = find_by_id(0),
    SubChannels = subchannels(Root),
    lists:map(fun(Channel) -> channelstate(Channel) end, [Root|SubChannels]).

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
channelstate(Channel) ->
    {channelstate, record_info:record_to_proplist(Channel, ?MODULE)}.

channelremove(Channel) ->
    {channelremove, record_info:record_to_proplist(Channel, ?MODULE)}.
