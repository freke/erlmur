-module(erlmur_crypto_benchmark_SUITE).

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    run_benchmark/2
]).

-include_lib("common_test/include/ct.hrl").

-define(KEY_SIZE, 16).
-define(BLOCK_SIZE, 16).

all() ->
    maps:keys(test_params()).

groups() ->
    [
        {encrypt_decrypt_performance, [], all()}
    ].

test_params() ->
    #{
        encrypt_16b => {encrypt, 16, 50000},
        encrypt_32b => {encrypt, 32, 50000},
        encrypt_128b => {encrypt, 128, 10000},
        encrypt_1kb => {encrypt, 1024, 1000},
        encrypt_4kb => {encrypt, 4096, 1000},
        encrypt_16kb => {encrypt, 16384, 100},
        encrypt_128kb => {encrypt, 131072, 50},
        encrypt_1mb => {encrypt, 1048576, 10},
        encrypt_4mb => {encrypt, 4 * 1048576, 10},

        decrypt_16b => {decrypt, 16, 50000},
        decrypt_32b => {decrypt, 32, 50000},
        decrypt_128b => {decrypt, 128, 10000},
        decrypt_1kb => {decrypt, 1024, 1000},
        decrypt_4kb => {decrypt, 4096, 1000},
        decrypt_16kb => {decrypt, 16384, 100},
        decrypt_128kb => {decrypt, 131072, 50},
        decrypt_1mb => {decrypt, 1048576, 10},
        decrypt_4mb => {decrypt, 4 * 1048576, 10}
    }.

init_per_suite(Config) ->
    logger:set_primary_config(level, warning),
    application:ensure_all_started(crypto),
    Config.

end_per_suite(Config) ->
    application:stop(crypto),
    Config.

init_per_testcase(_TestCase, Config) ->
    Key = crypto:strong_rand_bytes(?KEY_SIZE),
    InitialEncryptIV = <<0:128>>,
    InitialDecryptIV = <<0:128>>,
    InitialState = erlmur_crypto:init(Key, InitialDecryptIV, InitialEncryptIV),
    [{initial_state, InitialState} | Config].

end_per_testcase(_TestCase, Config) ->
    Config.

run_benchmark(TestName, Config) ->
    #{TestName := {Mode, Size, Iterations}} = test_params(),
    case Mode of
        encrypt -> encrypt_performance({Size, Iterations}, Config);
        decrypt -> decrypt_performance({Size, Iterations}, Config)
    end.

encrypt_16b(Config) -> run_benchmark(encrypt_16b, Config).
encrypt_32b(Config) -> run_benchmark(encrypt_32b, Config).
encrypt_128b(Config) -> run_benchmark(encrypt_128b, Config).
encrypt_1kb(Config) -> run_benchmark(encrypt_1kb, Config).
encrypt_4kb(Config) -> run_benchmark(encrypt_4kb, Config).
encrypt_16kb(Config) -> run_benchmark(encrypt_16kb, Config).
encrypt_128kb(Config) -> run_benchmark(encrypt_128kb, Config).
encrypt_1mb(Config) -> run_benchmark(encrypt_1mb, Config).
encrypt_4mb(Config) -> run_benchmark(encrypt_4mb, Config).

decrypt_16b(Config) -> run_benchmark(decrypt_16b, Config).
decrypt_32b(Config) -> run_benchmark(decrypt_32b, Config).
decrypt_128b(Config) -> run_benchmark(decrypt_128b, Config).
decrypt_1kb(Config) -> run_benchmark(decrypt_1kb, Config).
decrypt_4kb(Config) -> run_benchmark(decrypt_4kb, Config).
decrypt_16kb(Config) -> run_benchmark(decrypt_16kb, Config).
decrypt_128kb(Config) -> run_benchmark(decrypt_128kb, Config).
decrypt_1mb(Config) -> run_benchmark(decrypt_1mb, Config).
decrypt_4mb(Config) -> run_benchmark(decrypt_4mb, Config).

encrypt_performance({DataSize, NumIterations}, Config) ->
    InitialState = ?config(initial_state, Config),
    ct:log(info, "Running Encrypt Benchmark: DataSize=~p bytes, Iterations=~p", [
        DataSize, NumIterations
    ]),
    {FinalTime, _FinalState} = lists:foldl(
        fun(_I, {AccTime, State}) ->
            Plaintext = crypto:strong_rand_bytes(DataSize),
            Start = erlang:monotonic_time(nanosecond),
            {ok, _Cipher, NewState} = erlmur_crypto:encrypt(Plaintext, State),
            Time = erlang:monotonic_time(nanosecond) - Start,
            {AccTime + Time, NewState}
        end,
        {0, InitialState},
        lists:seq(1, NumIterations)
    ),
    AvgTimeNs = FinalTime / NumIterations,
    AvgTimeMs = erlang:convert_time_unit(round(AvgTimeNs), nanosecond, millisecond),
    ThroughputMBps = (DataSize * NumIterations / (FinalTime / 1.0e9)) / (1024 * 1024),
    ct:log(info, "Encrypt Performance:\n  Avg Time: ~p ns (~p ms)\n  Throughput: ~p MB/s", [
        AvgTimeNs, AvgTimeMs, ThroughputMBps
    ]).

decrypt_performance({DataSize, NumIterations}, Config) ->
    InitialState = ?config(initial_state, Config),
    ct:log(info, "Running Decrypt Benchmark: DataSize=~p bytes, Iterations=~p", [
        DataSize, NumIterations
    ]),
    {_DummyTime, _EncryptEndState, EncryptedPacketsRev} = lists:foldl(
        fun(_I, {AccTime, State, Acc}) ->
            Plaintext = crypto:strong_rand_bytes(DataSize),
            Start = erlang:monotonic_time(nanosecond),
            {ok, Cipher, NewState} = erlmur_crypto:encrypt(Plaintext, State),
            Time = erlang:monotonic_time(nanosecond) - Start,
            {AccTime + Time, NewState, [Cipher | Acc]}
        end,
        {0, InitialState, []},
        lists:seq(1, NumIterations)
    ),
    EncryptedPackets = lists:reverse(EncryptedPacketsRev),
    TotalTimeNs = 0,
    {FinalTime, _FinalState} = lists:foldl(
        fun(Packet, {AccTime, State}) ->
            Start = erlang:monotonic_time(nanosecond),
            {ok, _Plain, _Stats, NewState} = erlmur_crypto:decrypt(Packet, State),
            Time = erlang:monotonic_time(nanosecond) - Start,
            {AccTime + Time, NewState}
        end,
        {TotalTimeNs, InitialState},
        EncryptedPackets
    ),
    AvgTimeNs = FinalTime / NumIterations,
    AvgTimeMs = erlang:convert_time_unit(round(AvgTimeNs), nanosecond, millisecond),
    ThroughputMBps = (DataSize * NumIterations / (FinalTime / 1.0e9)) / (1024 * 1024),
    ct:log(info, "Decrypt Performance:\n  Avg Time: ~p ns (~p ms)\n  Throughput: ~p MB/s", [
        AvgTimeNs, AvgTimeMs, ThroughputMBps
    ]).
