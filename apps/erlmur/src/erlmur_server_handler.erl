-module(erlmur_server_handler).
-moduledoc """
Implements the mumble_server_behaviour for the Erlmur server.

This module bridges the mumble_protocol connection handling with the
Erlmur application logic, handling authentication and message routing.
""".

-behaviour(mumble_server_behaviour).

%% mumble_server_behaviour callbacks
-export([init/1, authenticate/2, handle_msg/2, get_caps/1]).

-record(state, {
    session_id :: pos_integer() | undefined,
    username :: binary() | undefined
}).

%%%===================================================================
%%% mumble_server_behaviour callbacks
%%%===================================================================

init(_Opts) ->
    {ok, #state{}}.

authenticate(#{message_type := 'Authenticate', username := Username}, State) ->
    %% MVP: Accept all users unconditionally
    {ok, SessionId} = erlmur_user_manager:register_user(self(), Username),
    UserInfo = #{session_id => SessionId, username => Username},
    logger:info("User ~s authenticated with session ~p", [Username, SessionId]),
    {ok, UserInfo, State#state{session_id = SessionId, username = Username}};
authenticate(Msg, State) ->
    logger:warning("Invalid authenticate message: ~p", [Msg]),
    {error, invalid_auth, State}.

handle_msg(#{message_type := connection_status, status := established, session_id := SessionId}, State) ->
    logger:notice("Session ~p established", [SessionId]),
    %% Ensure root channel exists for all connections
    erlmur_channel_manager:ensure_root_channel(),
    {ok, State};

handle_msg(#{message_type := connection_status, status := udp_verified, session_id := SessionId}, State) ->
    logger:notice("UDP verified for session ~p", [SessionId]),
    {ok, State};

handle_msg(#{message_type := connection_status, status := udp_lost, session_id := SessionId}, State) ->
    logger:notice("UDP lost for session ~p, falling back to TCP", [SessionId]),
    {ok, State};

handle_msg(#{message_type := 'TextMessage', message := Message}, State = #state{session_id = SessionId}) ->
    logger:debug("Text message received from session ~p", [SessionId]),
    erlmur_user_manager:broadcast_text(SessionId, Message),
    {ok, State};

handle_msg(#{message_type := 'UserState'} = Msg, State = #state{session_id = SessionId}) ->
    %% Handle channel join requests
    case maps:get(channel_id, Msg, undefined) of
        undefined -> ok;
        ChannelId ->
            logger:info("User ~p joining channel ~p", [SessionId, ChannelId]),
            erlmur_user_manager:update_user_channel(SessionId, ChannelId),
            %% Broadcast user state to all clients so they update their UI
            erlmur_user_manager:broadcast_user_state(SessionId)
    end,
    {ok, State};

handle_msg(#{message_type := 'ChannelState'} = Msg, State = #state{session_id = SessionId}) ->
    %% Delegate to channel manager for create or update
    case maps:get(channel_id, Msg, undefined) of
        undefined ->
            %% New channel creation - add actor (creator) to the message
            MsgWithActor = Msg#{actor => SessionId},
            erlmur_channel_manager:create_channel(MsgWithActor);
        ChannelId ->
            %% Existing channel update
            erlmur_channel_manager:update_channel(ChannelId, Msg)
    end,
    {ok, State};

handle_msg(#{message_type := 'ChannelRemove'} = Msg, State) ->
    %% Delegate to channel manager for removal
    erlmur_channel_manager:remove_channel(Msg),
    {ok, State};

handle_msg(#{message_type := 'PermissionQuery'}, State) ->
    %% Respond with default permissions for MVP
    mumble_server_conn:send(self(), #{
        message_type => 'PermissionQuery',
        channel_id => 0,
        permissions => 16#FFFFFFFF  %% All permissions
    }),
    {ok, State};

handle_msg(#{message_type := 'VoiceTarget'}, State) ->
    %% Ignore voice target setup for MVP
    {ok, State};

handle_msg(Msg, State) ->
    logger:debug("Unhandled message: ~p", [Msg]),
    {ok, State}.

get_caps(_State) ->
    #{
        major => 1,
        minor => 2,
        patch => 4,
        release => <<"Erlmur MVP">>,
        os => <<"Erlang/OTP">>,
        os_version => erlang:system_info(otp_release)
    }.
