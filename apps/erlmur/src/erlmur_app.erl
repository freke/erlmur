-module(erlmur_app).

-moduledoc "Main application entry point for the erlmur server.\n\nThis "
"module defines the application behavior and is responsible "
"for starting\nand stopping the main supervisor tree.".

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% Configure log level: ERLMUR_LOG_LEVEL env var > application env > default (info)
    LogLevel = case os:getenv("ERLMUR_LOG_LEVEL") of
        false -> application:get_env(erlmur, log_level, info);
        LevelStr -> list_to_atom(LevelStr)
    end,
    logger:set_primary_config(level, LogLevel),
    logger:notice("[erlmur] Log level set to: ~p", [LogLevel]),

    logger:debug("[erlmur] Starting erlmur application..."),
    PrivDir = code:priv_dir(erlmur),
    Port = application:get_env(erlmur, listen_port, 64738),
    CertFile = application:get_env(erlmur, cert_pem, filename:join(PrivDir, "server.pem")),
    KeyFile = application:get_env(erlmur, key_pem, filename:join(PrivDir, "server.key")),
    logger:debug("[erlmur] PrivDir=~p Port=~p CertFile=~p KeyFile=~p", [PrivDir, Port, CertFile, KeyFile]),
    logger:debug("[erlmur] Starting Mumble server with SSL cert: ~s", [CertFile]),
    %% Use erlmur_server_handler for production, not mock_mumble_handler
    case mumble:start_server(CertFile, KeyFile, Port, erlmur_server_handler) of
        {ok, ServerRef} ->
            logger:info("[erlmur] Mumble server started: ~p", [ServerRef]),
            erlmur_sup:start_link(),
            logger:notice("[erlmur] erlmur application started successfully"),
            {ok, self()};
        {error, Reason} ->
            logger:error("[erlmur] Failed to start Mumble server: ~p", [Reason]),
            {error, Reason}
    end.

stop(_State) ->
    ok.
