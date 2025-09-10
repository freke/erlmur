-module(erlmur_crypto_tests).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(DROP_THRESHOLD, 32).

erlmur_crypto_test_() ->
    logger:set_primary_config(level, warning),
    [
        {"Check OCB test vectors", ?setup(fun ocb_vectors_test/1)},
        {"Late message", ?setup(fun late_msg_test/1)},
        {"Message resent", ?setup(fun resend_msg_test/1)},
        {"Drop old message", ?setup(fun drop_old_packet_test/1)}
    ].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    ok.

stop(_) ->
    ok.

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
ocb_vectors_test(_) ->
    Key = binary:decode_hex(<<"000102030405060708090a0b0c0d0e0f">>),
    IV = binary:decode_hex(<<"000102030405060708090a0b0c0d0e0f">>),
    Vectors =
        [
            #{
                name => <<"OCB-AES-128-0B">>,
                plain => <<"">>,
                encrypted => <<"">>,
                tag => <<"BF3108130773AD5EC70EC69E7875A7B0">>
            },
            #{
                name => <<"OCB-AES-128-8B">>,
                plain => <<"0001020304050607">>,
                encrypted => <<"C636B3A868F429BB">>,
                tag => <<"A45F5FDEA5C088D1D7C8BE37CABC8C5C">>
            },
            #{
                name => <<"OCB-AES-128-16B">>,
                plain => <<"000102030405060708090A0B0C0D0E0F">>,
                encrypted => <<"52E48F5D19FE2D9869F0C4A4B3D2BE57">>,
                tag => <<"F7EE49AE7AA5B5E6645DB6B3966136F9">>
            },
            #{
                name => <<"OCB-AES-128-24B">>,
                plain => <<"000102030405060708090A0B0C0D0E0F1011121314151617">>,
                encrypted => <<"F75D6BC8B4DC8D66B836A2B08B32A636CC579E145D323BEB">>,
                tag => <<"A1A50F822819D6E0A216784AC24AC84C">>
            },
            #{
                name => <<"OCB-AES-128-32B">>,
                plain => <<"000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F">>,
                encrypted => <<"F75D6BC8B4DC8D66B836A2B08B32A636CEC3C555037571709DA25E1BB0421A27">>,
                tag => <<"09CA6C73F0B5C6C5FD587122D75F2AA3">>
            },
            #{
                name => <<"OCB-AES-128-40B">>,
                plain =>
                    <<
                        "000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F2021"
                        "222324252627"
                    >>,
                encrypted =>
                    <<
                        "F75D6BC8B4DC8D66B836A2B08B32A6369F1CD3C5228D79FD6C267F5F6AA7B231C7DF"
                        "B9D59951AE9C"
                    >>,
                tag => <<"9DB0CDF880F73E3E10D4EB3217766688">>
            }
        ],
    lists:flatmap(
        fun(
            #{
                name := _Name,
                plain := Plain,
                encrypted := Encrypted,
                tag := Tag
            }
        ) ->
            [
                ?_assertEqual(
                    #{
                        tag => binary:decode_hex(Tag),
                        ciphertext => binary:decode_hex(Encrypted)
                    },
                    erlmur_crypto:encrypt_ocb(
                        binary:decode_hex(Plain), Key, IV
                    )
                ),
                ?_assertEqual(
                    #{
                        tag => binary:decode_hex(Tag),
                        text => binary:decode_hex(Plain)
                    },
                    erlmur_crypto:decrypt_ocb(
                        binary:decode_hex(Encrypted), Key, IV
                    )
                )
            ]
        end,
        Vectors
    ).

late_msg_test(_) ->
    Plain = <<>>,
    S0 = erlmur_crypto:init(),
    State =
        erlmur_crypto:resync(
            erlmur_crypto:encrypt_iv(S0), S0
        ),

    {EncyptedMsgs, _} =
        lists:mapfoldl(
            fun(_, S) ->
                {ok, Encrypted, S1} = erlmur_crypto:encrypt(Plain, S),
                {Encrypted, S1}
            end,
            State,
            lists:seq(1, 30)
        ),
    DecryptedResults = lists:map(fun(E) -> erlmur_crypto:decrypt(E, State) end, EncyptedMsgs),
    Assert1 =
        lists:map(
            fun({{ok, <<>>, Stats, _State}, I}) ->
                ?_assertEqual(
                    {stats, 1, 0, I},
                    Stats
                )
            end,
            lists:zip(DecryptedResults, lists:seq(0, 29))
        ),
    LateEncryptedMsgs = lists:reverse(EncyptedMsgs),
    {DecryptedStates, _} =
        lists:mapfoldl(
            fun(E, S) ->
                {ok, <<>>, Stats, S1} = erlmur_crypto:decrypt(E, S),
                {{S1, Stats}, S1}
            end,
            State,
            LateEncryptedMsgs
        ),
    Assert2 =
        lists:map(
            fun({_, S}) ->
                ?_assertEqual(
                    {stats, 0, 1, -1},
                    S
                )
            end,
            tl(DecryptedStates)
        ),
    Assert1 ++ Assert2.

resend_msg_test(_) ->
    Plain = <<>>,
    S0 = erlmur_crypto:init(),
    State =
        erlmur_crypto:resync(
            erlmur_crypto:encrypt_iv(S0), S0
        ),
    {ok, Cipher0, EncryptState0} = erlmur_crypto:encrypt(Plain, State),
    {ok, Cipher1, EncryptState1} = erlmur_crypto:encrypt(Plain, EncryptState0),
    {ok, Cipher2, _EncryptState2} = erlmur_crypto:encrypt(Plain, EncryptState1),
    {ok, _Decrypted0, _Stats0, DecryptedState0} = erlmur_crypto:decrypt(Cipher0, State),
    {ok, _Decrypted1, _Stats1, DecryptedState1} = erlmur_crypto:decrypt(Cipher1, DecryptedState0),
    {ok, _Decrypted2, _Stats2, DecryptedState2} = erlmur_crypto:decrypt(Cipher2, DecryptedState1),
    [
        ?_assertEqual(
            {error, repeat, {stats, 0, 0, 0}, DecryptedState2},
            erlmur_crypto:decrypt(Cipher0, DecryptedState2)
        )
    ].

drop_old_packet_test(_) ->
    Plain = <<"Test">>,
    S0 = erlmur_crypto:init(),
    State =
        erlmur_crypto:resync(
            erlmur_crypto:encrypt_iv(S0), S0
        ),
    {_EncryptState, Packets} =
        lists:foldl(
            fun(_, {St, Acc}) ->
                {ok, C, St2} = erlmur_crypto:encrypt(Plain, St),
                {St2, [C | Acc]}
            end,
            {State, []},
            % Send more than threshold
            lists:seq(1, ?DROP_THRESHOLD + 2)
        ),
    {ok, _, _, FinalState} = erlmur_crypto:decrypt(hd(Packets), State),

    ?_assertMatch(
        {error, drop, {stats, 0, 0, 0}, FinalState},
        erlmur_crypto:decrypt(
            lists:last(Packets), FinalState
        )
    ).

%%%%%%%%%%%%%%%%%%%%%%
%%% HELP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%
