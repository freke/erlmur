-module(erlmur_crypto).
-moduledoc """
Provides authenticated encryption and decryption using AES-128 in OCB mode,
with replay protection and IV management for real-time communication systems.
""".

-export([
    init/0, init/3,
    key/1,
    decrypt_iv/1,
    encrypt_iv/1,
    resync/2,
    encrypt/2,
    decrypt/2
]).

% Max 255
-define(DROP_THRESHOLD, 32).
-define(HISTORY_WINDOW, ?DROP_THRESHOLD).

-record(history_entry, {seen = false :: boolean(), value = 0 :: byte()}).
-record(stats, {good = 0 :: integer(), late = 0 :: integer(), lost = 0 :: integer()}).

-type history_entry() :: #history_entry{}.
-type history() :: array:array(history_entry()).
-type key() :: <<_:128>>.
-type nonce() :: <<_:128>>.
-type stats() :: #stats{}.

-record(crypto_state, {
    key :: key(),
    decrypt_iv :: nonce(),
    encrypt_iv :: nonce(),
    history = array:new(?HISTORY_WINDOW, {default, #history_entry{}}) :: history()
}).

-opaque crypto_state() :: #crypto_state{}.
-export_type([crypto_state/0, key/0, nonce/0]).

-doc """
Initializes a new crypto state with a randomly generated AES-128 key and IVs.
This is the standard entry point for setting up the encryption/decryption state.
""".
-spec init() -> crypto_state().
init() ->
    Key = crypto:strong_rand_bytes(16),
    DIV = crypto:strong_rand_bytes(16),
    EIV = crypto:strong_rand_bytes(16),
    init(Key, DIV, EIV).

-doc """
Initializes a crypto state using the provided AES-128 key, decrypt IV, and encrypt IV.
All inputs must be 16-byte binaries (128 bits).
""".
-spec init(key(), nonce(), nonce()) -> crypto_state().
init(Key, DIV, EIV) when
    is_binary(Key),
    byte_size(Key) =:= 16,
    is_binary(DIV),
    byte_size(DIV) =:= 16,
    is_binary(EIV),
    byte_size(EIV) =:= 16
->
    #crypto_state{
        key = Key,
        decrypt_iv = DIV,
        encrypt_iv = EIV
    }.

-doc """
Returns the AES-128 encryption key from the given crypto state.
""".
-spec key(crypto_state()) -> key().
key(State) ->
    State#crypto_state.key.

-doc """
Returns the current decryption IV (initialization vector) from the crypto state.
""".
-spec decrypt_iv(crypto_state()) -> nonce().
decrypt_iv(State) ->
    State#crypto_state.decrypt_iv.

-doc """
Returns the current encryption IV (initialization vector) from the crypto state.
""".
-spec encrypt_iv(crypto_state()) -> nonce().
encrypt_iv(State) ->
    State#crypto_state.encrypt_iv.

-doc """
Updates the crypto state's decryption IV (initialization vector) to a new value,
effectively resynchronizing the expected nonce for incoming packets.
""".
-spec resync(nonce(), crypto_state()) -> crypto_state().
resync(Div, State) ->
    State#crypto_state{decrypt_iv = Div}.

-doc """
Encrypts a binary payload using AES-128 in OCB mode and returns the authenticated ciphertext.

```
Function encrypt(Buffer, State):
    Key ← State.key
    IV ← State.encrypt_iv

    NewIV ← IV + 1
    NonceByte ← least_significant_byte(NewIV)

    Offset ← AES_ECB_Encrypt(Key, NewIV)

    Ciphertext ← []
    Checksum ← zero block

    While Buffer has full 128-bit block:
        Offset ← s2(Offset)
        Block ← next 128-bit chunk from Buffer
        Encrypted ← Offset XOR AES_ECB_Encrypt(Key, Offset XOR Block)
        Ciphertext ← Ciphertext ++ Encrypted
        Checksum ← Checksum XOR Block

    If Buffer has remaining partial block:
        Offset ← s2(Offset)
        LengthBits = bit_length(PartialBlock)
        Pad ← AES_ECB_Encrypt(Key, Offset XOR LengthBits)
        PaddedPartialBlock ← PartialBlock ++ zero_padding_to_128_bits
        Padded ← Pad XOR PaddedPartialBlock
        N ← byte_length(PartialBlock)
        EncryptedTrunc ← First N bytes of Padded
        Ciphertext ← Ciphertext ++ EncryptedTrunc
        Checksum ← Checksum XOR PaddedPartialBlock

    Tag ← AES_ECB_Encrypt(Key, (s2(Offset) XOR Offset) XOR Checksum)
    Packet ← NonceByte ++ first 3 bytes of Tag ++ Ciphertext

    Return {ok, Packet, State with encrypt_iv = NewIV}
```
""".
-spec encrypt(binary(), crypto_state()) -> {ok, binary(), crypto_state()}.
encrypt(Buf, State = #crypto_state{key = Key, encrypt_iv = <<IV:128/unsigned-little-integer>>}) ->
    NewIV = <<(IV + 1):128/unsigned-little-integer>>,
    logger:debug("IV ~p NewIV ~p", [
        binary:encode_hex(<<IV:128/unsigned-little-integer>>), binary:encode_hex(NewIV)
    ]),
    #{ciphertext := CipherText, tag := Tag} = encrypt_ocb(Buf, Key, NewIV),
    <<TagPrefix:24/bits, _/bits>> = Tag,
    <<NonceByte:8, _/binary>> = NewIV,
    Header = <<NonceByte:8, TagPrefix:24/bits>>,
    Packet = <<Header/binary, CipherText/binary>>,
    {ok, Packet, State#crypto_state{encrypt_iv = NewIV}}.

-doc """
Decrypts a binary packet using AES-128 in OCB mode and validates its authenticity
using a truncated 24-bit (3-byte) authentication tag.

The packet header consists of the least significant byte of the 128-bit nonce,
followed by the first three bytes of the authentication tag.

This function expects packets structured as follows:

```mermaid
packet-beta
title OCB-AES-128 Encrypted Packet
0-7: "Nonce (Counter)"
8-31: "Authentication Tag (fingerprint)"
32-159: "Ciphertext (encrypted payload of variable length)"
```

The 8-bit nonce byte is used to reconstruct the full 128-bit IV (Initialization Vector) for decryption.
The tag is compared against the computed OCB tag to ensure authenticity.

Replay protection is enforced using a sliding window based on the nonce
and an internal decryption history buffer.

```
Function decrypt(Packet, State):
    Extract NonceByte, TagPrefix (3 bytes), Ciphertext from Packet

    DecryptNonce ← State.decrypt_iv
    History ← State.history

    Diff ← wraparound_diff(NonceByte, DecryptNonce)
    Nonce ← DecryptNonce + Diff

    Classification ← classify_nonce(Diff, NonceByte, History)

    Match Classification:
        Case repeat:
            Return {error, repeat, State}
        Case drop:
            Return {error, drop, State}
        Case late or lost or in_order:
            Update State.decrypt_iv
            Update History
            Update Stats Accordendly

    Offset ← AES_ECB_Encrypt(Key, Nonce)
    Plaintext ← []
    Checksum ← zero block

    While Ciphertext has full 128-bit block:
        Offset ← s2(Offset)
        Block ← next 128-bit chunk from Ciphertext
        Decrypted ← Offset XOR AES_ECB_Decrypt(Key, Offset XOR Block)
        Plaintext ← Plaintext ++ Decrypted
        Checksum ← Checksum XOR Decrypted

    If Ciphertext has remaining partial block:
        Ciphertext_PartialBlock ← remaining_bits_from(Ciphertext) // Define this variable explicitly
        Offset ← s2(Offset)
        LengthBits ← bit_length(Ciphertext_PartialBlock) // Bit length of the remaining ciphertext
        Pad ← AES_ECB_Encrypt(Key, Offset XOR LengthBits)
        Ciphertext_PartialBlock_Padded ← Ciphertext_PartialBlock ++ zero_padding_to_128_bits 
        PaddedPlaintextBlock ← Pad XOR Ciphertext_PartialBlock_Padded
        N ← byte_length(Ciphertext_PartialBlock) 
        DecryptedTrunc ← First N bytes of PaddedPlaintextBlock
        Plaintext ← Plaintext ++ DecryptedTrunc
        Checksum ← Checksum XOR PaddedPlaintextBlock

    Tag ← AES_ECB_Encrypt(Key, (s2(Offset) XOR Offset) XOR Checksum)

    If TagPrefix == first 3 bytes of Tag:
        Return {ok, Plaintext, UpdatedState}
    Else:
        Return {error, invalid_tag, State}
```
""".
-spec decrypt(binary(), crypto_state()) ->
    {ok, binary(), stats(), crypto_state()}
    | {error, repeat | drop | invalid_tag, stats(), crypto_state()}.
decrypt(
    <<Nonce0:8/bits, Tag:24/bits, Rest/bits>> = Packet,
    State = #crypto_state{
        key = Key,
        decrypt_iv = Nonce,
        history = History
    }
) ->
    logger:debug(
        "Decrypting packet ~p nonce: ~p tag: ~p",
        [binary:encode_hex(Packet), Nonce0, Tag]
    ),

    maybe
        logger:debug("History ~p", [History]),
        Classification = classify_nonce(Nonce0, Nonce, History),
        {ok, UseNonce, Stats, SaveState} ?=
            update_state_by_classification(Classification, Nonce0, State),
        logger:debug("Updated History ~p", [SaveState#crypto_state.history]),
        #{tag := TagOut, text := Plain} = decrypt_ocb(Rest, Key, UseNonce),
        logger:debug("Plain ~p", [binary:encode_hex(Plain)]),
        ok ?= validate_tag(Tag, TagOut),
        {ok, Plain, Stats, SaveState}
    else
        {repeat, NewState} ->
            {error, repeat, #stats{}, NewState};
        {drop, NewState} ->
            {error, drop, #stats{}, NewState};
        invalid_tag ->
            {error, invalid_tag, #stats{}, State}
    end.

%%%%%%
%%% Private
%%%%%%

encrypt_ocb(Plain, Key, IV) ->
    Options = [{encrypt, true}],
    StateIV = crypto:crypto_init(aes_128_ecb, Key, Options),
    Offset = crypto:crypto_update(StateIV, IV),
    <<>> = crypto:crypto_final(StateIV),
    StateEnc = crypto:crypto_init(aes_128_ecb, Key, Options),
    encrypt_blocks(Plain, Offset, StateEnc, <<0:128>>, []).

decrypt_ocb(Encrypted, Key, IV) ->
    StateIV = crypto:crypto_init(aes_128_ecb, Key, [{encrypt, true}]),
    Offset = crypto:crypto_update(StateIV, IV),
    <<>> = crypto:crypto_final(StateIV),
    StateDec = crypto:crypto_init(aes_128_ecb, Key, [{encrypt, false}]),
    StateEnc = crypto:crypto_init(aes_128_ecb, Key, [{encrypt, true}]),
    decrypt_blocks(Encrypted, Offset, StateDec, StateEnc, <<0:128>>, []).

encrypt_blocks(<<Block:128/bits, Rest/bits>>, Offset, StateEnc, Checksum, Acc) when
    Rest =/= <<>>
->
    NewOffset = s2(Offset),
    EncryptedBlock =
        crypto:exor(
            crypto:crypto_update(StateEnc, crypto:exor(NewOffset, Block)), NewOffset
        ),
    NewChecksum = crypto:exor(Checksum, Block),
    encrypt_blocks(Rest, NewOffset, StateEnc, NewChecksum, [EncryptedBlock | Acc]);
encrypt_blocks(<<>>, Offset, StateEnc, Checksum, Acc) ->
    NewOffset = s2(Offset),
    Pad = crypto:crypto_update(StateEnc, NewOffset),
    % Replaced PaddedPlain with Pad
    NewChecksum = crypto:exor(Checksum, Pad),
    Tag = crypto:crypto_update(StateEnc, compute_tag(NewOffset, NewChecksum)),
    <<>> = crypto:crypto_final(StateEnc),
    #{tag => Tag, ciphertext => iolist_to_binary(lists:reverse(Acc))};
encrypt_blocks(Partial, Offset, StateEnc, Checksum, Acc) ->
    NewOffset = s2(Offset),
    LenBits = bit_size(Partial),
    LenBytes = (LenBits + 7) div 8,

    PadInput = crypto:exor(<<LenBits:128>>, NewOffset),
    Pad = crypto:crypto_update(StateEnc, PadInput),
    <<_Drop:LenBits/bits, PadRemainder/bits>> = Pad,
    PaddedPlain = <<Partial/bits, PadRemainder/bits>>,
    Encrypted = crypto:exor(Pad, PaddedPlain),
    EncryptedTrunc = binary:part(Encrypted, 0, LenBytes),

    NewChecksum = crypto:exor(Checksum, PaddedPlain),
    Tag = crypto:crypto_update(StateEnc, compute_tag(NewOffset, NewChecksum)),
    <<>> = crypto:crypto_final(StateEnc),
    #{tag => Tag, ciphertext => iolist_to_binary(lists:reverse([EncryptedTrunc | Acc]))}.

decrypt_blocks(<<Block:128/bits, Rest/bits>>, Offset, StateDec, StateEnc, Checksum, Acc) when
    Rest =/= <<>>
->
    NewOffset = s2(Offset),
    PlainBlock =
        crypto:exor(
            crypto:crypto_update(StateDec, crypto:exor(NewOffset, Block)), NewOffset
        ),
    NewChecksum = crypto:exor(Checksum, PlainBlock),
    decrypt_blocks(
        Rest,
        NewOffset,
        StateDec,
        StateEnc,
        NewChecksum,
        [PlainBlock | Acc]
    );
decrypt_blocks(<<>>, Offset, _StateDec, StateEnc, Checksum, Acc) ->
    NewOffset = s2(Offset),
    Pad = crypto:crypto_update(StateEnc, NewOffset),
    NewChecksum = crypto:exor(Checksum, Pad),
    Tag = crypto:crypto_update(StateEnc, compute_tag(NewOffset, NewChecksum)),
    <<>> = crypto:crypto_final(StateEnc),
    #{tag => Tag, text => iolist_to_binary(lists:reverse(Acc))};
decrypt_blocks(Partial, Offset, _StateDec, StateEnc, Checksum, Acc) ->
    NewOffset = s2(Offset),
    LenBits = bit_size(Partial),
    LenBytes = (LenBits + 7) div 8,

    PadInput = crypto:exor(<<LenBits:128>>, NewOffset),
    Pad = crypto:crypto_update(StateEnc, PadInput),
    <<_Drop:LenBits/bits, PadRemainder/bits>> = Pad,
    PaddedEncrypted = <<Partial/bits, PadRemainder/bits>>,
    Plain = crypto:exor(Pad, PaddedEncrypted),
    PlainTrunc = binary:part(Plain, 0, LenBytes),

    PaddedPlain = <<PlainTrunc/bits, PadRemainder/bits>>,
    NewChecksum = crypto:exor(Checksum, PaddedPlain),
    Tag = crypto:crypto_update(StateEnc, compute_tag(NewOffset, NewChecksum)),
    <<>> = crypto:crypto_final(StateEnc),
    #{tag => Tag, text => iolist_to_binary(lists:reverse([PlainTrunc | Acc]))}.

compute_tag(Offset, Checksum) ->
    crypto:exor(
        crypto:exor(Offset, s2(Offset)), Checksum
    ).

s2(<<B:128>>) ->
    Carry = B band (1 bsl 127) =/= 0,
    Shifted = (B bsl 1) band 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF,
    Final =
        if
            Carry ->
                Shifted bxor 16#87;
            true ->
                Shifted
        end,
    <<Final:128>>.

wraparound_diff(<<A:8, _/bits>>, <<B:8, _/bits>>) ->
    Diff = (A - B) band 255,
    if
        Diff > 127 -> Diff - 256;
        true -> Diff
    end.

classify_nonce(Nonce, DecryptNonce, History) ->
    Diff = wraparound_diff(Nonce, DecryptNonce),
    <<DN:128/unsigned-little-integer>> = DecryptNonce,
    CandidateNonce = <<(DN + Diff):128/unsigned-little-integer>>,

    case Diff of
        1 ->
            logger:debug("Decrypt Msg in order"),
            {in_order, CandidateNonce};
        N when N > 1 ->
            logger:debug("Lost ~p Msg", [N]),
            {lost, N - 1, CandidateNonce};
        N when N > -?DROP_THRESHOLD ->
            % Check for repeat
            case is_seen(Nonce, CandidateNonce, History) of
                true ->
                    logger:debug("Repeted Msg"),
                    {repeat, DecryptNonce};
                false ->
                    logger:debug("Late Msg"),
                    {late, CandidateNonce}
            end;
        Diff ->
            logger:debug("Drop Msg ~p", [Diff]),
            {drop, DecryptNonce}
    end.

update_state_by_classification({repeat, _}, _Nonce0, State) ->
    {repeat, State};
update_state_by_classification(
    {in_order, CandidateNonce},
    Nonce0,
    State = #crypto_state{history = History}
) ->
    {ok, CandidateNonce, #stats{good = 1}, State#crypto_state{
        decrypt_iv = CandidateNonce,
        history = update_history(Nonce0, CandidateNonce, History)
    }};
update_state_by_classification({drop, _CandidateNonce}, _Nonce0, State) ->
    {drop, State};
update_state_by_classification(
    {late, CandidateNonce},
    Nonce0,
    State = #crypto_state{history = History}
) ->
    {ok, CandidateNonce, #stats{lost = -1, late = 1}, State#crypto_state{
        history = update_history(Nonce0, CandidateNonce, History)
    }};
update_state_by_classification(
    {lost, LostCount, CandidateNonce},
    Nonce0,
    State = #crypto_state{history = History}
) ->
    {ok, CandidateNonce, #stats{good = 1, lost = LostCount}, State#crypto_state{
        decrypt_iv = CandidateNonce,
        history = update_history(Nonce0, CandidateNonce, History)
    }}.

validate_tag(<<ExpectedTag:24/bits>>, <<Tag3:24/bits, _/bits>>) ->
    case (Tag3 =:= ExpectedTag) of
        true ->
            ok;
        false ->
            logger:error("MAC mismatch: expected ~p got ~p", [
                binary:encode_hex(ExpectedTag), binary:encode_hex(Tag3)
            ]),
            invalid_tag
    end.

update_history(<<Index:8>>, <<Nonce:128>>, History) ->
    HistoryIndex = Index band (?HISTORY_WINDOW - 1),
    N = ((Nonce bsr 8) band 255),
    Entry = #history_entry{seen = true, value = N},
    array:set(HistoryIndex, Entry, History).

is_seen(<<Index:8>>, <<Nonce:128>>, History) ->
    HistoryIndex = Index band (?HISTORY_WINDOW - 1),
    N = ((Nonce bsr 8) band 255),
    case array:get(HistoryIndex, History) of
        #history_entry{seen = true, value = N} -> true;
        _ -> false
    end.
