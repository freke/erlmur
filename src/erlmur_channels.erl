%%%-------------------------------------------------------------------
%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 31 Mar 2013 by David AAberg <davabe@hotmail.com>
%%%-------------------------------------------------------------------
-module(erlmur_channels).

-include("mumble_pb.hrl").

-export([init/0,channel/2,list/1,update/2,remove/2]).

-record(state,{last_id,channels}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
init() ->
    #state{last_id = 0,
	   channels = dict:store(0, #channelstate{ channel_id=0,parent=0, name= <<"Root">>}, dict:new())}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
channel(Key, #state{channels=Channels}) ->
    dict:fetch(Key,Channels).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
list(#state{channels=Channels}) ->
    dict:fetch_keys(Channels).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
update(ChannelState = #channelstate{channel_id=undefined},
       #state{last_id=LastId,channels=Channels} = State) ->
    error_logger:info_report([{erlmur_channels,update},
			      "New channel",
			      {channel, LastId+1},
			      {name,ChannelState#channelstate.name}]),
    CS = ChannelState#channelstate{channel_id=LastId+1},
    NC = dict:store(LastId+1,CS,Channels),
    [erlmur_client:channelstate(P,CS) || P <- erlmur_users:list_clients()],
    State#state{last_id=LastId+1,channels=NC}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
remove(Channel = #channelremove{channel_id=Id},
       #state{channels=Channels} = State) ->

    SubChannels = dict:filter(fun(_,#channelstate{parent=SubId}) -> SubId =:= Id end, Channels),
    NewState = lists:foldl(fun(SubId,Acc) -> remove(#channelremove{channel_id=SubId},Acc) end, 
			   State, 
			   dict:fetch_keys(SubChannels)),
		
    error_logger:info_report([{erlmur_cahnnels,remove},
			      {channel, Id}]),
    lists:foreach(fun(U) -> erlmur_users:move_to_channel(U,0) end, erlmur_users:in_channel(Id)),
    NC = dict:erase(Id,Channels),
    [erlmur_client:channelremove(P,Channel) || P <- erlmur_users:list_clients()],
    NewState#state{channels=NC}.
