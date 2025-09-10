-module(erlmur_sup).

-moduledoc """
The main supervisor for the erlmur application.

It starts and supervises all the core components of the server.
""".

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-define(DEF_PORT, 64738).
-define(SSL_OPTIONS, [
    {versions, ['tlsv1.2', 'tlsv1.3']},
    {ciphers, [
        "TLS_AES_256_GCM_SHA384",
        "TLS_CHACHA20_POLY1305_SHA256",
        "TLS_AES_128_GCM_SHA256",
        "ECDHE-ECDSA-AES256-GCM-SHA384",
        "ECDHE-RSA-AES256-GCM-SHA384",
        "ECDHE-ECDSA-AES128-GCM-SHA256",
        "ECDHE-RSA-AES128-GCM-SHA256"
    ]},
    {verify, verify_peer},
    {cacerts, public_key:cacerts_get()},
    {verify_fun, {fun verify_peer/3, []}},
    {fail_if_no_peer_cert, true}
]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, App} = application:get_application(),
    ListenPort = application:get_env(App, listen_port, ?DEF_PORT),
    CertPem = application:get_env(App, cert_pem, "cert.pem"),
    KeyPem = application:get_env(App, key_pem, "key.pem"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort, CertPem, KeyPem]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port, CertPem, KeyPem]) ->
    Children = [
        #{
            id => erlmur_server,
            start => {erlmur_server, start_link, []}
        },
        #{
            id => erlmur_udp_server,
            start => {erlmur_udp_server, start_link, [Port]}
        },
        #{
            id => erlmur_session_registry,
            start => {erlmur_session_registry, start_link, []}
        },
        #{
            id => pg_channels,
            start => {pg, start_link, [pg_erlmur]}
        },
        ranch:child_spec(
            erlmur_ssl,
            ranch_ssl,
            [
                {port, Port},
                {certfile, CertPem},
                {keyfile, KeyPem}
                | ?SSL_OPTIONS
            ],
            erlmur_session,
            []
        )
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

verify_peer(_OtpCert, {bad_cert, selfsigned_peer}, UserState) ->
    AllowSelfSigned = application:get_env(erlmur, allow_selfsigned, false),
    if
        AllowSelfSigned ->
            logger:debug("Using selfsigned cert..."),
            {valid, UserState};
        true ->
            {fail, {bad_cert, selfsigned_peer}}
    end;
verify_peer(Cert, {bad_cert, Reason}, _UserState) ->
    logger:warning("Peer cert verification failed: ~p ~p", [Reason, Cert]),
    {fail, {bad_cert, Reason}};
verify_peer(_OtpCert, {extension, _}, UserState) ->
    {unknown, UserState};
verify_peer(_OtpCert, valid, UserState) ->
    {valid, UserState};
verify_peer(_OtpCert, valid_peer, UserState) ->
    {valid, UserState}.
