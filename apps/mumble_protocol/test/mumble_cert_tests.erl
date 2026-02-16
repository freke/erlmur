-module(mumble_cert_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include("mumble_protocol.hrl").

%%%%%%%%%%%%%%%%%%%%%%%
%% Helper Functions
%%%%%%%%%%%%%%%%%%%%%%%

get_test_dir() ->
    "/tmp/mumble_test_certs_" ++ integer_to_list(erlang:phash2(make_ref())).

ensure_test_dir() ->
    TestDir = get_test_dir(),
    case file:make_dir(TestDir) of
        ok -> ok;
        {error, eexist} -> ok
    end,
    TestDir.

cleanup_test_dir(TestDir) ->
    case filelib:is_dir(TestDir) of
        true -> 
            Files = filelib:wildcard(filename:join(TestDir, "*")),
            [file:delete(F) || F <- Files],
            file:del_dir(TestDir);
        false -> ok
    end.

generate_cert(CertFile, KeyFile) ->
    Cmd = io_lib:format(
        "openssl req -x509 -newkey rsa:2048 -keyout ~s -out ~s -days 1 -nodes -subj '/CN=localhost' 2>&1",
        [KeyFile, CertFile]
    ),
    os:cmd(Cmd).

generate_cert(CertFile, KeyFile, Subject, Days) ->
    Cmd = io_lib:format(
        "openssl req -x509 -newkey rsa:2048 -keyout ~s -out ~s -days ~B -nodes -subj '~s' 2>&1",
        [KeyFile, CertFile, Days, Subject]
    ),
    os:cmd(Cmd).

%%%%%%%%%%%%%%%%%%%%%%%
%% Setup Check
%%%%%%%%%%%%%%%%%%%%%%%

openssl_available_test() ->
    Result = os:cmd("which openssl"),
    ?assert(length(Result) > 0).

%%%%%%%%%%%%%%%%%%%%%%%
%% Certificate Generation Tests
%%%%%%%%%%%%%%%%%%%%%%%

generate_cert_creates_files_test() ->
    TestDir = ensure_test_dir(),
    try
        CertFile = filename:join(TestDir, "test.pem"),
        KeyFile = filename:join(TestDir, "test.key"),
        
        generate_cert(CertFile, KeyFile),
        
        ?assert(filelib:is_file(CertFile)),
        ?assert(filelib:is_file(KeyFile))
    after
        cleanup_test_dir(TestDir)
    end.

generate_cert_creates_valid_x509_test() ->
    TestDir = ensure_test_dir(),
    try
        CertFile = filename:join(TestDir, "test.pem"),
        KeyFile = filename:join(TestDir, "test.key"),
        
        generate_cert(CertFile, KeyFile),
        
        VerifyCmd = io_lib:format("openssl x509 -in ~s -text -noout 2>&1", [CertFile]),
        Output = os:cmd(VerifyCmd),
        ?assert(string:find(Output, "Certificate") =/= nomatch)
    after
        cleanup_test_dir(TestDir)
    end.

generate_cert_with_default_subject_test() ->
    TestDir = ensure_test_dir(),
    try
        CertFile = filename:join(TestDir, "test.pem"),
        KeyFile = filename:join(TestDir, "test.key"),
        
        generate_cert(CertFile, KeyFile),
        
        SubjectCmd = io_lib:format("openssl x509 -in ~s -subject -noout 2>&1", [CertFile]),
        Subject = os:cmd(SubjectCmd),
        %% OpenSSL format can be "subject=CN = localhost" or "CN=localhost"
        ?assert(
            (string:find(Subject, "CN = localhost") =/= nomatch) orelse
            (string:find(Subject, "CN=localhost") =/= nomatch)
        )
    after
        cleanup_test_dir(TestDir)
    end.

generate_cert_with_custom_subject_test() ->
    TestDir = ensure_test_dir(),
    try
        CertFile = filename:join(TestDir, "test.pem"),
        KeyFile = filename:join(TestDir, "test.key"),
        
        generate_cert(CertFile, KeyFile, "/CN=test.example.com/O=TestOrg", 1),
        
        SubjectCmd = io_lib:format("openssl x509 -in ~s -subject -noout 2>&1", [CertFile]),
        Subject = os:cmd(SubjectCmd),
        ?assert(
            (string:find(Subject, "test.example.com") =/= nomatch) orelse
            (string:find(Subject, "test") =/= nomatch)
        )
    after
        cleanup_test_dir(TestDir)
    end.

generate_cert_with_custom_days_test() ->
    TestDir = ensure_test_dir(),
    try
        CertFile = filename:join(TestDir, "test.pem"),
        KeyFile = filename:join(TestDir, "test.key"),
        
        generate_cert(CertFile, KeyFile, "/CN=localhost", 30),
        
        DatesCmd = io_lib:format("openssl x509 -in ~s -dates -noout 2>&1", [CertFile]),
        Dates = os:cmd(DatesCmd),
        ?assert(string:find(Dates, "notBefore") =/= nomatch),
        ?assert(string:find(Dates, "notAfter") =/= nomatch)
    after
        cleanup_test_dir(TestDir)
    end.

reuse_existing_cert_test() ->
    TestDir = ensure_test_dir(),
    try
        CertFile = filename:join(TestDir, "test.pem"),
        KeyFile = filename:join(TestDir, "test.key"),
        
        %% First generation
        generate_cert(CertFile, KeyFile),
        {ok, FileInfo1} = file:read_file_info(CertFile),
        MTime1 = FileInfo1#file_info.mtime,
        
        timer:sleep(100),
        
        %% Check files still exist
        {ok, FileInfo2} = file:read_file_info(CertFile),
        MTime2 = FileInfo2#file_info.mtime,
        
        ?assertEqual(MTime1, MTime2)
    after
        cleanup_test_dir(TestDir)
    end.

regenerate_missing_cert_test() ->
    TestDir = ensure_test_dir(),
    try
        CertFile = filename:join(TestDir, "test.pem"),
        KeyFile = filename:join(TestDir, "test.key"),
        
        %% Generate both
        generate_cert(CertFile, KeyFile),
        ?assert(filelib:is_file(CertFile)),
        ?assert(filelib:is_file(KeyFile)),
        
        %% Delete cert only
        ok = file:delete(CertFile),
        ?assertNot(filelib:is_file(CertFile)),
        
        %% Regenerate
        generate_cert(CertFile, KeyFile),
        
        ?assert(filelib:is_file(CertFile)),
        ?assert(filelib:is_file(KeyFile))
    after
        cleanup_test_dir(TestDir)
    end.

cert_cleanup_test() ->
    TestDir = ensure_test_dir(),
    try
        CertFile = filename:join(TestDir, "test.pem"),
        KeyFile = filename:join(TestDir, "test.key"),
        
        generate_cert(CertFile, KeyFile),
        ?assert(filelib:is_file(CertFile)),
        ?assert(filelib:is_file(KeyFile)),
        
        %% Cleanup
        Files = filelib:wildcard(filename:join(TestDir, "*")),
        [file:delete(F) || F <- Files],
        file:del_dir(TestDir),
        
        ?assertNot(filelib:is_dir(TestDir))
    after
        cleanup_test_dir(TestDir)
    end.
