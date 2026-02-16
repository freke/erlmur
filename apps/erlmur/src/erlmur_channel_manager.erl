-module(erlmur_channel_manager).

-moduledoc """
Manages channels for the Erlmur server.

This gen_server tracks all channels and provides APIs for creating,
updating, removing, and broadcasting channel state to all connected users.
Channels are stored in an ETS table with auto-generated IDs (starting from 1,
with 0 reserved for the root channel).
""".

-behaviour(gen_server).

-include("erlmur.hrl").

%% API
-export([start_link/0]).
-export([create_channel/1, remove_channel/1, get_channel/1, get_all_channels/0, update_channel/2]).
-export([ensure_root_channel/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    channels :: ets:tid(),
    next_channel_id = 1 :: pos_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc """
Create a new channel from a ChannelState message map.
Auto-generates channel ID. Logs error and returns ok if parent doesn't exist.
Input: ChannelState map with fields: parent, name, description, position, max_users, temporary, links
Output: ok
""".
-spec create_channel(map()) -> ok.
create_channel(ChannelState) ->
    gen_server:call(?MODULE, {create_channel, ChannelState}).

-doc """
Remove a channel by ID.
Logs error and returns ok if channel doesn't exist.
Input: Channel ID or ChannelRemove map with channel_id field
Output: ok
""".
-spec remove_channel(non_neg_integer() | map()) -> ok.
remove_channel(#{channel_id := ChannelId}) ->
    remove_channel(ChannelId);
remove_channel(ChannelId) ->
    gen_server:call(?MODULE, {remove_channel, ChannelId}).

-doc """
Get a channel by ID.
Input: Channel ID
Output: {ok, #channel{}} | {error, not_found}
""".
-spec get_channel(non_neg_integer()) -> {ok, #channel{}} | {error, not_found}.
get_channel(ChannelId) ->
    gen_server:call(?MODULE, {get_channel, ChannelId}).

-doc """
Get all channels.
Output: List of #channel{} records
""".
-spec get_all_channels() -> [#channel{}].
get_all_channels() ->
    gen_server:call(?MODULE, get_all_channels).

-doc """
Update an existing channel's properties.
Logs error and returns ok if channel doesn't exist.
Input: Channel ID and ChannelState map with updated fields
Output: ok
""".
-spec update_channel(non_neg_integer(), map()) -> ok.
update_channel(ChannelId, ChannelState) ->
    gen_server:call(?MODULE, {update_channel, ChannelId, ChannelState}).

-doc """
Ensure the root channel (ID 0) exists.
Creates it if it doesn't exist.
Output: ok
""".
-spec ensure_root_channel() -> ok.
ensure_root_channel() ->
    gen_server:call(?MODULE, ensure_root_channel).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Channels = ets:new(erlmur_channels, [set, private, {keypos, #channel.id}]),
    logger:info("Channel manager initialized"),
    {ok, #state{channels = Channels}}.

handle_call({create_channel, ChannelState}, _From, State = #state{channels = Channels, next_channel_id = NextId}) ->
    ParentId = maps:get(parent, ChannelState, undefined),
    
    %% Validate parent exists (if specified and not root)
    case validate_parent(ParentId, Channels) of
        ok ->
            ChannelId = case maps:get(channel_id, ChannelState, undefined) of
                undefined -> NextId;
                Id -> Id
            end,
            
            Channel = #channel{
                id = ChannelId,
                parent_id = ParentId,
                name = maps:get(name, ChannelState, <<"Unnamed">>),
                description = maps:get(description, ChannelState, undefined),
                position = maps:get(position, ChannelState, 0),
                max_users = maps:get(max_users, ChannelState, undefined),
                temporary = maps:get(temporary, ChannelState, false),
                links = maps:get(links, ChannelState, [])
            },
            
            ets:insert(Channels, Channel),
            logger:info("Channel '~s' created with ID ~p", [Channel#channel.name, ChannelId]),
            
            %% Broadcast to all users
            broadcast_channel_state(ChannelState#{channel_id => ChannelId}),
            
            NewNextId = case ChannelId >= NextId of
                true -> ChannelId + 1;
                false -> NextId
            end,
            
            {reply, ok, State#state{next_channel_id = NewNextId}};
        {error, parent_not_found} ->
            ChannelName = maps:get(name, ChannelState, <<"Unnamed">>),
            logger:error("Cannot create channel '~s': parent ~p not found", [ChannelName, ParentId]),
            {reply, ok, State}
    end;

handle_call({remove_channel, ChannelId}, _From, State = #state{channels = Channels}) ->
    case ChannelId of
        0 ->
            logger:error("Cannot remove root channel (ID 0)"),
            {reply, ok, State};
        _ ->
            case ets:lookup(Channels, ChannelId) of
                [#channel{name = Name}] ->
                    ets:delete(Channels, ChannelId),
                    logger:info("Channel '~s' (ID ~p) removed", [Name, ChannelId]),
                    
                    %% Broadcast removal to all users
                    broadcast_channel_remove(ChannelId),
                    
                    {reply, ok, State};
                [] ->
                    logger:error("Cannot remove channel ~p: not found", [ChannelId]),
                    {reply, ok, State}
            end
    end;

handle_call({get_channel, ChannelId}, _From, State = #state{channels = Channels}) ->
    case ets:lookup(Channels, ChannelId) of
        [Channel] -> {reply, {ok, Channel}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call(get_all_channels, _From, State = #state{channels = Channels}) ->
    AllChannels = ets:tab2list(Channels),
    {reply, AllChannels, State};

handle_call({update_channel, ChannelId, ChannelState}, _From, State = #state{channels = Channels}) ->
    case ets:lookup(Channels, ChannelId) of
        [ExistingChannel] ->
            UpdatedChannel = ExistingChannel#channel{
                name = maps:get(name, ChannelState, ExistingChannel#channel.name),
                description = maps:get(description, ChannelState, ExistingChannel#channel.description),
                position = maps:get(position, ChannelState, ExistingChannel#channel.position),
                max_users = maps:get(max_users, ChannelState, ExistingChannel#channel.max_users),
                temporary = maps:get(temporary, ChannelState, ExistingChannel#channel.temporary),
                links = maps:get(links, ChannelState, ExistingChannel#channel.links)
            },
            
            ets:insert(Channels, UpdatedChannel),
            logger:info("Channel '~s' (ID ~p) updated", [UpdatedChannel#channel.name, ChannelId]),
            
            %% Broadcast update to all users
            broadcast_channel_state(ChannelState#{channel_id => ChannelId}),
            
            {reply, ok, State};
        [] ->
            logger:error("Cannot update channel ~p: not found", [ChannelId]),
            {reply, ok, State}
    end;

handle_call(ensure_root_channel, _From, State = #state{channels = Channels}) ->
    case ets:lookup(Channels, 0) of
        [] ->
            RootChannel = #channel{
                id = 0,
                parent_id = undefined,
                name = <<"Root">>,
                description = undefined,
                position = 0,
                max_users = undefined,
                temporary = false,
                links = []
            },
            ets:insert(Channels, RootChannel),
            logger:info("Root channel (ID 0) created"),
            {reply, ok, State};
        [_] ->
            {reply, ok, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

validate_parent(undefined, _Channels) ->
    ok;
validate_parent(0, _Channels) ->
    ok;
validate_parent(ParentId, Channels) ->
    case ets:lookup(Channels, ParentId) of
        [_] -> ok;
        [] -> {error, parent_not_found}
    end.

broadcast_channel_state(ChannelState) ->
    Msg = ChannelState#{message_type => 'ChannelState'},
    broadcast_to_all_users(Msg).

broadcast_channel_remove(ChannelId) ->
    Msg = #{
        message_type => 'ChannelRemove',
        channel_id => ChannelId
    },
    broadcast_to_all_users(Msg).

broadcast_to_all_users(Msg) ->
    Users = erlmur_user_manager:get_all_users(),
    lists:foreach(
        fun(#user{pid = Pid}) ->
            mumble_server_conn:send(Pid, Msg)
        end,
        Users
    ).
