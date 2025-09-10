-module(prop_erlmur_crypto).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

prop_ocb_encrypt_decrypt() ->
    logger:set_primary_config(level, warning),
    ?FORALL(
        {Plain, Key, IV},
        {binary(), binary(16), binary(16)},
        begin
            #{ciphertext := Cipher, tag := Tag1} = erlmur_crypto:encrypt_ocb(Plain, Key, IV),
            #{text := Decrypted, tag := Tag2} = erlmur_crypto:decrypt_ocb(Cipher, Key, IV),
            Plain == Decrypted andalso Tag1 == Tag2
        end
    ).

prop_encrypt_decrypt() ->
    logger:set_primary_config(level, warning),
    ?FORALL(
        {Plain, Key, IV},
        {binary(), binary(16), binary(16)},
        begin
            State = erlmur_crypto:init(Key, IV, IV),
            {ok, Cipher, _EncryptState} = erlmur_crypto:encrypt(Plain, State),
            {ok, Decrypted, {stats, 1, 0, 0}, _DecryptedState} = erlmur_crypto:decrypt(
                Cipher, State
            ),
            Plain == Decrypted
        end
    ).

prop_late_packet_behavior() ->
    logger:set_primary_config(level, warning),
    ?FORALL(
        {Messages, Key, IV},
        {non_empty(list(binary())), binary(16), binary(16)},
        begin
            State = erlmur_crypto:init(Key, IV, IV),
            Length = length(Messages),
            {EncyptedMsgs, _} =
                lists:mapfoldl(
                    fun(Plain, S) ->
                        {ok, Encrypted, S1} = erlmur_crypto:encrypt(Plain, S),
                        {Encrypted, S1}
                    end,
                    State,
                    Messages
                ),
            DecryptedResults =
                lists:map(fun(E) -> erlmur_crypto:decrypt(E, State) end, EncyptedMsgs),
            lists:map(
                fun({{ok, _, Stats, _}, I}) ->
                    {stats, 1, 0, I} = Stats
                end,
                lists:zip(DecryptedResults, lists:seq(0, Length - 1))
            ),
            {LateEncryptedMsgs, LostEncryptedMsgs} = split_list(32, lists:reverse(EncyptedMsgs)),
            {LateDecryptedStates, LateDecryptedState} =
                lists:mapfoldl(
                    fun(E, S) ->
                        {ok, _, Stats, S1} = erlmur_crypto:decrypt(E, S),
                        {{S1, Stats}, S1}
                    end,
                    State,
                    LateEncryptedMsgs
                ),
            LateDecryptedState =
                lists:foldl(
                    fun(E, S) ->
                        {error, drop, {stats, 0, 0, 0}, S1} = erlmur_crypto:decrypt(E, S),
                        S1
                    end,
                    LateDecryptedState,
                    LostEncryptedMsgs
                ),

            lists:foreach(
                fun
                    ({{_S, {stats, 0, 1, -1}}, _}) ->
                        ok;
                    ({{_S, {stats, 1, 0, Recoved}}, I}) ->
                        I =:= Recoved + 1
                end,
                lists:zip(
                    LateDecryptedStates,
                    lists:seq(0, length(LateDecryptedStates) - 1)
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
