-module(mumble_api_tests).
-include_lib("eunit/include/eunit.hrl").
-include("mumble_protocol.hrl").

%%%%%%%%%%%%%%%%%%%%%%%
%% Test Setup/Teardown
%%%%%%%%%%%%%%%%%%%%%%%

setup() ->
    %% Ensure OpenSSL is available
    case os:cmd("which openssl") of
        [] -> error(openssl_not_available);
        _ -> ok
    end,
    ok.

teardown(_) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%
%% Validation Tests (Don't require ranch)
%%%%%%%%%%%%%%%%%%%%%%%

start_server_validation_tests() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"start_server error when cert file not found",
         fun start_server_error_cert_not_found_test/0},
        {"start_server error when key file not found",
         fun start_server_error_key_not_found_test/0},
        {"start_server error when only cert_file provided",
         fun start_server_error_cert_only_test/0},
        {"start_server error when only key_file provided",
         fun start_server_error_key_only_test/0},
        {"start_server error when both files undefined",
         fun start_server_error_both_undefined_test/0}
     ]
    }.

start_server_error_cert_not_found_test() ->
    %% This should return {error, {cert_file_not_found, _}}
    ?assertEqual(
        {error, {cert_file_not_found, "/nonexistent/cert.pem"}},
        mumble:start_server("/nonexistent/cert.pem", "/tmp/key.pem")
    ).

start_server_error_key_not_found_test() ->
    %% Create cert file but not key file
    CertFile = "/tmp/test_cert_only.pem",
    ok = file:write_file(CertFile, "test cert"),
    try
        ?assertEqual(
            {error, {key_file_not_found, "/nonexistent/key.pem"}},
            mumble:start_server(CertFile, "/nonexistent/key.pem")
        )
    after
        file:delete(CertFile)
    end.

start_server_error_cert_only_test() ->
    %% Only cert_file provided - should return cert_key_mismatch error
    ?assertEqual(
        {error, cert_key_mismatch},
        mumble:start_server("/tmp/cert.pem", undefined)
    ).

start_server_error_key_only_test() ->
    %% Only key_file provided - should return cert_key_mismatch error
    ?assertEqual(
        {error, cert_key_mismatch},
        mumble:start_server(undefined, "/tmp/key.pem")
    ).

start_server_error_both_undefined_test() ->
    %% Both files undefined - should return missing_cert_and_key error
    ?assertEqual(
        {error, missing_cert_and_key},
        mumble:start_server(undefined, undefined)
    ).

%%%%%%%%%%%%%%%%%%%%%%%
%% Integration Tests (Require ranch - run in CT instead)
%%%%%%%%%%%%%%%%%%%%%%%

%% These tests are commented out because they require ranch to be running,
%% which is not available in EUnit context. They are tested in mumble_SUITE.erl instead.
%%
%% start_server_with_certs_test() ->
%%     CertFile = "/tmp/test_api_cert.pem",
%%     KeyFile = "/tmp/test_api_key.pem",
%%     ok = file:write_file(CertFile, "test cert"),
%%     ok = file:write_file(KeyFile, "test key"),
%%     try
%%         ?assertMatch({ok, {mumble_server, _, _}}, mumble:start_server(CertFile, KeyFile))
%%     after
%%         file:delete(CertFile),
%%         file:delete(KeyFile)
%%     end.

%%%%%%%%%%%%%%%%%%%%%%%
%% Stop Listener Tests
%%%%%%%%%%%%%%%%%%%%%%%

stop_listener_tests() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"stop_listener with invalid ref returns error",
         fun stop_listener_invalid_ref_test/0},
        {"stop_listener is idempotent with invalid ref",
         fun stop_listener_idempotent_test/0}
     ]
    }.

stop_listener_invalid_ref_test() ->
    ?assertEqual(
        {error, invalid_server_ref},
        mumble:stop_listener(invalid_ref)
    ).

stop_listener_idempotent_test() ->
    Ref = make_ref(),
    ?assertEqual(
        {error, invalid_server_ref},
        mumble:stop_listener(Ref)
    ),
    ?assertEqual(
        {error, invalid_server_ref},
        mumble:stop_listener(Ref)
    ).

%%%%%%%%%%%%%%%%%%%%%%%
%% Export All Tests
%%%%%%%%%%%%%%%%%%%%%%%

all_test_() ->
    [
        start_server_validation_tests(),
        stop_listener_tests()
    ].
