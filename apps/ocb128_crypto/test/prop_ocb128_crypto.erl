-module(prop_ocb128_crypto).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ocb128_crypto/include/ocb128_crypto.hrl").

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

prop_ocb_encrypt_decrypt() ->
    logger:set_primary_config(level, warning),
    ?FORALL(
        {Plain, Key, IV},
        {binary(), binary(16), binary(16)},
        begin
            #{ciphertext := Cipher, tag := Tag1} = ocb128_crypto:encrypt_ocb(Plain, Key, IV),
            #{text := Decrypted, tag := Tag2} = ocb128_crypto:decrypt_ocb(Cipher, Key, IV),
            Plain == Decrypted andalso Tag1 == Tag2
        end
    ).

prop_encrypt_decrypt() ->
    logger:set_primary_config(level, warning),
    ?FORALL(
        {Plain, Key, IV},
        {binary(), binary(16), binary(16)},
        begin
            State = ocb128_crypto:init(Key, IV, IV),
            {ok, Cipher, _EncryptState} = ocb128_crypto:encrypt(Plain, State),
            {ok, Decrypted, DecryptedState} = ocb128_crypto:decrypt(
                Cipher, State
            ),
            Stats = ocb128_crypto:stats(DecryptedState),
            Plain == Decrypted andalso Stats#crypto_stats.good == 1 andalso Stats#crypto_stats.late == 0 andalso Stats#crypto_stats.lost == 0
        end
    ).

prop_late_packet_behavior() ->
    logger:set_primary_config(level, warning),
    ?FORALL(
        {Messages, Key, IV},
        {non_empty(list(binary())), binary(16), binary(16)},
        begin
            State = ocb128_crypto:init(Key, IV, IV),
            Length = length(Messages),
            {EncyptedMsgs, _} =
                lists:mapfoldl(
                    fun(Plain, S) ->
                        {ok, Encrypted, S1} = ocb128_crypto:encrypt(Plain, S),
                        {Encrypted, S1}
                    end,
                    State,
                    Messages
                ),
            DecryptedResults =
                lists:map(fun(E) -> ocb128_crypto:decrypt(E, State) end, EncyptedMsgs),
            lists:map(
                fun({{ok, _, S}, I}) ->
                    Stats = ocb128_crypto:stats(S),
                    1 = Stats#crypto_stats.good,
                    0 = Stats#crypto_stats.late,
                    I = Stats#crypto_stats.lost
                end,
                lists:zip(DecryptedResults, lists:seq(0, Length - 1))
            ),
            {LateEncryptedMsgs, LostEncryptedMsgs} = split_list(32, lists:reverse(EncyptedMsgs)),
            {LateDecryptedStates, LateDecryptedState} =
                lists:mapfoldl(
                    fun(E, S) ->
                        {ok, _, S1} = ocb128_crypto:decrypt(E, S),
                        Stats = ocb128_crypto:stats(S1),
                        {{S1, Stats}, S1}
                    end,
                    State,
                    LateEncryptedMsgs
                ),
            LateDecryptedState =
                lists:foldl(
                    fun(E, S) ->
                        {error, drop, S1} = ocb128_crypto:decrypt(E, S),
                        S1
                    end,
                    LateDecryptedState,
                    LostEncryptedMsgs
                ),

            N = length(LateDecryptedStates),
            lists:foreach(
                fun({{_S, Stats}, I}) ->
                    1 = Stats#crypto_stats.good,
                    I = Stats#crypto_stats.late,
                    (N - 1 - I) =:= Stats#crypto_stats.lost
                end,
                lists:zip(
                    LateDecryptedStates,
                    lists:seq(0, N - 1)
                )
            ),
            true
        end
    ).

%%%%%%%%%%%%%%%%%%%%%%
%%% HELP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%

split_list(N, List) ->
    case length(List) =< N of
        true ->
            {List, []};
        false ->
            lists:split(N, List)
    end.
