-module(mumble_cert).

-moduledoc """
Certificate management module for Mumble protocol.

This module handles all certificate-related operations including validation,
auto-generation of self-signed certificates, and file path management.

This is an internal module - use mumble.erl for the public API.
""".

-export([validate_cert_files/2, ensure_auto_certs/2, generate_self_signed_cert/4]).

-include("mumble_protocol.hrl").

-define(DEFAULT_CERT_DAYS, 365).
-define(DEFAULT_CERT_SUBJECT, "/CN=localhost").

-doc """
Validate that certificate and key files exist and are both provided.

Input:
  - CertFile: Path to certificate file (can be undefined)
  - KeyFile: Path to private key file (can be undefined)

Returns:
  - ok: Both files exist
  - {error, missing_cert_and_key}: Both are undefined
  - {error, cert_key_mismatch}: One is provided but not the other
  - {error, {cert_file_not_found, Path}}: Certificate file doesn't exist
  - {error, {key_file_not_found, Path}}: Key file doesn't exist
""".
-spec validate_cert_files(file:filename_all() | undefined, file:filename_all() | undefined) ->
    ok | {error, term()}.
validate_cert_files(CertFile, KeyFile) ->
    case {CertFile, KeyFile} of
        {undefined, undefined} ->
            {error, missing_cert_and_key};
        {undefined, _} ->
            {error, cert_key_mismatch};
        {_, undefined} ->
            {error, cert_key_mismatch};
        {CF, KF} ->
            case filelib:is_file(CF) of
                false ->
                    {error, {cert_file_not_found, CF}};
                true ->
                    case filelib:is_file(KF) of
                        false ->
                            {error, {key_file_not_found, KF}};
                        true ->
                            ok
                    end
            end
    end.

-doc """
Ensure auto-generated certificates exist, creating them if needed.

Input:
  - Subject: Certificate subject line (e.g., "/CN=localhost")
  - Days: Validity period in days

Returns:
  - {ok, CertFile, KeyFile}: Paths to the certificate files
  - {error, Reason}: Certificate generation failed

If certificates already exist in priv_dir, they are reused.
""".
-spec ensure_auto_certs(string(), pos_integer()) ->
    {ok, file:filename_all(), file:filename_all()} | {error, term()}.
ensure_auto_certs(Subject, Days) ->
    PrivDir = code:priv_dir(mumble_protocol),
    CertFile = filename:join(PrivDir, "auto_server.pem"),
    KeyFile = filename:join(PrivDir, "auto_server.key"),

    case {filelib:is_file(CertFile), filelib:is_file(KeyFile)} of
        {true, true} ->
            %% Existing certs found, reuse them
            {ok, CertFile, KeyFile};
        _ ->
						logger:notice("Generating new self-signed certificates"),
            generate_self_signed_cert(CertFile, KeyFile, Subject, Days)
    end.

-doc """
Generate a self-signed certificate using OpenSSL.

Input:
  - CertFile: Path where certificate file should be written
  - KeyFile: Path where private key file should be written
  - Subject: Certificate subject line
  - Days: Validity period in days

Returns:
  - {ok, CertFile, KeyFile}: Success, returns paths to created files
  - {error, Reason}: Certificate generation failed
""".
-spec generate_self_signed_cert(file:filename_all(), file:filename_all(), string(), pos_integer()) ->
    {ok, file:filename_all(), file:filename_all()} | {error, term()}.
generate_self_signed_cert(CertFile, KeyFile, Subject, Days) ->
    maybe
			{ok,_} ?= {filelib:ensure_dir(KeyFile),KeyFile},
			{ok,_} ?= {filelib:ensure_dir(CertFile),CertFile},
			OpenSSL = os:find_executable("openssl"),
      DaysStr = integer_to_list(Days),
      Port = open_port({spawn_executable, OpenSSL}, [
	      {args, ["req", "-x509", "-newkey", "rsa:2048",
                "-keyout", KeyFile, "-out", CertFile,
                "-days", DaysStr, "-nodes", "-subj", Subject]},
        exit_status, stderr_to_stdout, binary
      ]),
			Output = collect_port_output(Port, []),
			logger:error("~p",[Output]),
      %% Verify results
      {true, _} ?= {filelib:is_file(CertFile), Output},
      {true, _} ?= {filelib:is_file(KeyFile), Output},

      {ok, CertFile, KeyFile}
    else
      {false, Reason} -> 
				{error, {cert_generation_failed, string:trim(Reason)}};
			{{error, Reason}, File} -> 
        {error, {auto_cert_failed, Reason, File}}
    end.

collect_port_output(Port, Data) ->
    receive
        {Port, {exit_status, _Status}} -> {ok, iolist_to_binary(Data)};
        {Port, {data, NewData}} -> collect_port_output(Port, [NewData|Data])
    after 30000 ->
        port_close(Port),
        {error, timeout}
    end.
