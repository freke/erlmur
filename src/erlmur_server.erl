-module(erlmur_server).

-moduledoc """
Manages the overall state and configuration of the Erlmur server.


This module handles server-wide settings, authentication, and delegates
requests to other specialized modules for processing.
""".

-behaviour(gen_server).

-include("erlmur.hrl").

%% API
-export([
    start_link/0,
    version/0,
    codecversion/0, codecversion/2,
    config/0,
    usercount/0,
    list_banlist/0
]).
%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {version, codec_version = #codec_version{}, server_config = #server_config{}}).

-opaque version() :: #version{}.

-export_type([version/0]).

-opaque codec_version() :: #codec_version{}.

-export_type([codec_version/0]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

version() ->
    gen_server:call(?SERVER, version).

codecversion() ->
    gen_server:call(?SERVER, codecversion).

codecversion(Codec, Opus) ->
    gen_server:cast(?SERVER, {codecversion, Codec, Opus}).

config() ->
    gen_server:call(?SERVER, serverconfig).

usercount() ->
    gen_server:call(?SERVER, usercount).

list_banlist() ->
    [].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Nodes = [node()],
    ChannelTables = erlmur_channel_store:init(Nodes),
    UserTables = erlmur_user_store:init(Nodes),
    {OsFamily, OsName} = os:type(),
    OsNameString = io_lib:format("~w-~w", [OsFamily, OsName]),
    OsVersion =
        case os:version() of
            {OsMajor, OsMinor, OsRelease} ->
                io_lib:format("~w.~w.~w", [OsMajor, OsMinor, OsRelease]);
            V ->
                V
        end,
    {ok, VsnStr} = application:get_key(erlmur, vsn),
    Version =
        #version{
            major = ?MUMBLE_PROTOCOL_VERSION_MAJOR,
            minor = ?MUMBLE_PROTOCOL_VERSION_MINOR,
            patch = ?MUMBLE_PROTOCOL_VERSION_PATCH,
            release = iolist_to_binary(io_lib:format("erlmur ~s", [VsnStr])),
            os = iolist_to_binary(OsNameString),
            os_version = iolist_to_binary(OsVersion)
        },
    ok = mnesia:wait_for_tables(UserTables ++ ChannelTables, 5000),
    erlmur_channel_store:create_default(),
    {ok, #state{version = Version}}.

handle_call(version, _From, State) ->
    {reply, State#state.version, State};
handle_call(codecversion, _From, State) ->
    {reply, State#state.codec_version, State};
handle_call(serverconfig, _From, State) ->
    {reply, State#state.server_config, State};
handle_call(usercount, _From, State) ->
    NumUsers = proplists:get_value(workers, supervisor:count_children(erlmur_session_sup)),
    {reply, NumUsers, State};
handle_call(Request, _From, State) ->
    error_logger:info_report([{erlmur_server, handle_call}, {unhandled_request, Request}]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast({codecversion, CeltVersions, Opus}, State = #state{codec_version = CurrentCodec}) ->
    % This is a temporary, simplified negotiation.
    % A proper implementation would require storing the capabilities of all
    % connected clients and finding a common codec.
    NewCodec =
        if
            Opus andalso CurrentCodec#codec_version.opus ->
                CurrentCodec;
            true ->
                % Find a common CELT codec. For now, we just prefer the server's alpha.
                ServerAlpha = CurrentCodec#codec_version.alpha,
                case lists:member(ServerAlpha, CeltVersions) of
                    true ->
                        CurrentCodec#codec_version{opus = false};
                    false ->
                        % Client does not support server's preferred CELT.
                        % What to do? For now, just log and keep current.
                        logger:warning(
                            "Client does not support server's preferred CELT codec. Client versions: ~p",
                            [CeltVersions]
                        ),
                        CurrentCodec
                end
        end,

    if
        NewCodec =/= CurrentCodec ->
            logger:info("Codec changed to ~p. Broadcasting.", [NewCodec]),
            Pids = pg:get_members(pg_erlmur, users),
            lists:foreach(
                fun(Pid) ->
                    erlmur_tcp_message:send(Pid, {codec_version, NewCodec})
                end,
                Pids
            ),
            {noreply, State#state{codec_version = NewCodec}};
        true ->
            {noreply, State}
    end;
handle_cast(Msg, State) ->
    error_logger:info_report([{erlmur_server, handle_cast}, {"Unhandle msg", Msg}]),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:info_report([{erlmur_server, handle_info}, {"Unhandle info", Info}]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
