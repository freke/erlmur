-module(erlmur_channel_manager).

-moduledoc """
Manages channels for the Erlmur server.

This gen_server tracks all channels and provides APIs for creating,
updating, removing, and broadcasting channel state to all connected users.
Channels are stored in an ETS table with auto-generated IDs (starting from 1,
with 0 reserved for the root channel).
""".

-behaviour(gen_server).

-include_lib("erlmur/include/erlmur.hrl").

%% API
-export([start_link/0]).
-export([create_channel/1, remove_channel/1, get_channel/1, get_all_channels/0, update_channel/2]).
-export([ensure_root_channel/0]).
-export([check_and_cleanup_temporary_channel/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    channels :: ets:table(),
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
Input: ChannelState map with fields: parent, name, description, position, max_users, temporary, links, actor
""".
-spec create_channel(map()) -> ok.
create_channel(ChannelState) ->
    gen_server:call(?MODULE, {create_channel, ChannelState}).

-doc """
Remove a channel by ID.
Logs error and returns ok if channel doesn't exist.
Returns {error, has_users} if channel or subchannels have users.
Input: Channel ID or ChannelRemove map with channel_id field
""".
-spec remove_channel(non_neg_integer() | map()) -> ok | {error, has_users}.
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
""".
-spec update_channel(non_neg_integer(), map()) -> ok.
update_channel(ChannelId, ChannelState) ->
    gen_server:call(?MODULE, {update_channel, ChannelId, ChannelState}).

-doc """
Ensure the root channel (ID 0) exists.
Creates it if it doesn't exist.
""".
-spec ensure_root_channel() -> ok.
ensure_root_channel() ->
    gen_server:call(?MODULE, ensure_root_channel).

-doc """
Check if a temporary channel should be removed and remove it.
Called when a user leaves or disconnects.
Removes the channel if it's temporary, empty, and has no subchannels.
Input: Channel ID
""".
-spec check_and_cleanup_temporary_channel(non_neg_integer()) -> ok.
check_and_cleanup_temporary_channel(ChannelId) ->
    gen_server:cast(?MODULE, {check_and_cleanup_temporary_channel, ChannelId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Channels = ets:new(erlmur_channels, [set, private, {keypos, #channel.id}]),
    logger:info("Channel manager initialized"),
    {ok, #state{channels = Channels}}.

handle_call({create_channel, ChannelState}, _From, State = #state{channels = Channels, next_channel_id = NextId}) ->
    ParentId = maps:get(parent, ChannelState, undefined),
    Actor = maps:get(actor, ChannelState, undefined),
    IsTemporary = maps:get(temporary, ChannelState, false),

    %% Validate parent exists (if specified and not root)
    case validate_parent(ParentId, Channels) of
        ok ->
            ChannelId = NextId,

            Channel = #channel{
                id = ChannelId,
                parent_id = ParentId,
                name = maps:get(name, ChannelState, ~"Unnamed"),
                description = maps:get(description, ChannelState, undefined),
                position = maps:get(position, ChannelState, 0),
                max_users = maps:get(max_users, ChannelState, undefined),
                temporary = IsTemporary,
                links = maps:get(links, ChannelState, [])
            },

            ets:insert(Channels, Channel),
            logger:info("Channel '~s' created with ID ~p", [Channel#channel.name, ChannelId]),

            %% Broadcast to all users
            broadcast_channel_state(ChannelState#{channel_id => ChannelId}),

            %% If temporary, move creator to the new channel
            case IsTemporary of
                true when Actor =/= undefined ->
                    erlmur_user_manager:update_user_channel(Actor, ChannelId),
                    erlmur_user_manager:broadcast_user_state(Actor);
                _ ->
                    ok
            end,

            NewNextId = case ChannelId >= NextId of
                true -> ChannelId + 1;
                false -> NextId
            end,

            {reply, ok, State#state{next_channel_id = NewNextId}};
        {error, parent_not_found} ->
            ChannelName = maps:get(name, ChannelState, ~"Unnamed"),
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
                [#channel{name = Name, parent_id = ParentId}] ->
                    %% Get all subchannels recursively
                    Subchannels = get_subchannels(ChannelId, Channels),
                    AllChannelsToRemove = [ChannelId | Subchannels],

                    %% Check if any channel has users
                    case check_channels_have_users(AllChannelsToRemove) of
                        true ->
                            logger:warning("Cannot remove channel '~s' (ID ~p) or its subchannels: has users", [Name, ChannelId]),
                            {reply, {error, has_users}, State};
                        false ->
                            %% Remove subchannels first (bottom-up), then the parent
                            lists:foreach(
                                fun(Id) ->
                                    case ets:lookup(Channels, Id) of
                                        [#channel{name = SubName}] ->
                                            ets:delete(Channels, Id),
                                            logger:info("Channel '~s' (ID ~p) removed", [SubName, Id]),
                                            broadcast_channel_remove(Id);
                                        [] -> ok
                                    end
                                end,
                                lists:reverse(AllChannelsToRemove)
                            ),
                            logger:info("Channel '~s' (ID ~p) and ~p subchannels removed", [Name, ChannelId, length(Subchannels)]),
                            
                            %% Check if parent is temporary and should be cleaned up
                            case ParentId of
                                undefined -> ok;
                                0 -> ok;
                                _ -> 
                                    %% Parent might need cleanup now
                                    check_and_cleanup_temporary_channel(ParentId)
                            end,
                            
                            {reply, ok, State}
                    end;
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
                name = ~"Root",
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

handle_cast({check_and_cleanup_temporary_channel, ChannelId}, State = #state{channels = Channels}) ->
    case ets:lookup(Channels, ChannelId) of
        [#channel{temporary = true}] ->
            %% Check if channel has users
            case erlmur_user_manager:get_users_in_channel(ChannelId) of
                [] ->
                    %% No users, check for subchannels
                    Subchannels = get_subchannels(ChannelId, Channels),
                    case Subchannels of
                        [] ->
                            %% No users, no subchannels - remove it
                            ets:delete(Channels, ChannelId),
                            logger:info("Temporary channel (ID ~p) auto-removed", [ChannelId]),
                            broadcast_channel_remove(ChannelId);
                        _ ->
                            logger:debug("Temporary channel (ID ~p) has subchannels, not removing", [ChannelId])
                    end;
                _Users ->
                    logger:debug("Temporary channel (ID ~p) still has users, not removing", [ChannelId])
            end;
        [#channel{temporary = false}] ->
            %% Not a temporary channel, ignore
            ok;
        [] ->
            %% Channel doesn't exist, ignore
            ok
    end,
    {noreply, State};
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

%% Recursively find all subchannels of a given channel
direct_subchannels(ParentId, Channels) ->
    ets:foldl(
        fun(#channel{id = Id, parent_id = Pid}, Acc) when Pid == ParentId ->
                [Id | Acc];
           (_, Acc) -> Acc
        end,
        [],
        Channels
    ).

get_subchannels(ParentId, Channels) ->
    Direct = direct_subchannels(ParentId, Channels),
    get_subchannels_recursive(Direct, Channels, []).

get_subchannels_recursive([], _Channels, Acc) ->
    Acc;
get_subchannels_recursive([Id | Rest], Channels, Acc) ->
    Subs = direct_subchannels(Id, Channels),
    get_subchannels_recursive(Rest ++ Subs, Channels, [Id | Acc]).

%% Check if any channel in the list has users
check_channels_have_users(ChannelIds) ->
    lists:any(
        fun(Id) ->
            Users = erlmur_user_manager:get_users_in_channel(Id),
            length(Users) > 0
        end,
        ChannelIds
    ).
