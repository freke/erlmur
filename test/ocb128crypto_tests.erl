%%%-------------------------------------------------------------------
%%% File    : ocb128_crypto_server_tests.erl
%%% Author  : David Åberg <freke@hotmail.com>
%%% Description : 
%%%
%%% Created :  26 Dec 2010 by David Åberg <freke@hotmail.com>
%%%-------------------------------------------------------------------
-module(ocb128crypto_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

test_crypto_setup() ->
    application:start(crypto).

test_crypto_teardown(_) ->
    application:stop(crypto).

bin_to_hexstr(Bin) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) ||
		      X <- binary_to_list(Bin)]).

hexstr_to_bin(S) ->
    hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
    list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
    {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
    hexstr_to_bin(T, [V | Acc]).

crypto_test_() ->
    {
      setup, 
      fun test_crypto_setup/0, 
      fun test_crypto_teardown/1,
      fun(_) ->
	      test_crypto(hexstr_to_bin("C636B3A868F429BB"),
	                  hexstr_to_bin("A45F5FDEA5C088D1D7C8BE37CABC8C5C"),
	                  hexstr_to_bin("0001020304050607"))
		  ++
		  test_crypto(iolist_to_binary(""),
			      hexstr_to_bin("BF3108130773AD5EC70EC69E7875A7B0"),
			      iolist_to_binary(""))
		  ++
		  test_crypto(iolist_to_binary(""),
			      hexstr_to_bin("000102030405060708090A0B0C0D0E0F"),
			      hexstr_to_bin("BF3108130773AD5EC70EC69E7875A7B0"))
      end
    }.

timecrypto_test_() ->
    {
      setup,
      fun test_crypto_setup/0, 
      fun test_crypto_teardown/1,
      fun(_) ->
	      K1 = ocb128crypt:new_key(),
	      K2 = setelement(1, K1, <<63,42,28,240,161,191,20,150,200,51,75,196,47,181,153,155>>),
	      K3 = setelement(2, K2, <<253,150,98,246,19,99,253,71,24,251,21,116,183,124,73,100>>),
	      K4 = setelement(3, K3, <<253,150,98,246,19,99,253,71,24,251,21,116,183,124,73,100>>),
	      Plain = <<32,161,207,147,247,4>>,
	      {Encrypted,_} = ocb128crypt:encrypt(K4,Plain),
	      {Decrypted,_} = ocb128crypt:decrypt(K4,Encrypted),
	      [?_assertEqual(Plain, Decrypted),
	       ?_assertEqual(<<254,127,0,51,65,10,3,17,216,36>>,Encrypted)]
      end
    }.

proper_module_test() ->
  ?assertEqual(
    [],
    proper:module(?MODULE, [long_result])).
 
test_crypto(Ciphertext, Tag, Plain) ->
    K1 = ocb128crypt:new_key(),
    Key = hexstr_to_bin("000102030405060708090A0B0C0D0E0F"),
    IV = hexstr_to_bin("000102030405060708090A0B0C0D0E0F"),
    K2 = setelement(1, K1, Key),
    K3 = setelement(2, K2, IV),
    K4 = setelement(3, K3, IV),
    {Encrypted,_} = ocb128crypt:encrypt(K4,Plain),
    {Decrypted,_} = ocb128crypt:decrypt(K4,Encrypted),
    [?_assertEqual(Plain, Decrypted)].

prop_crypto() ->
    K1 = ocb128crypt:new_key(),
    proper:quickcheck(
      proper:numtests(
	1000, 
	?TRAPEXIT(
	   ?FORALL({Key, IV, Msg}, 
		   {binary(16), binary(16), resize(400,binary())}, 
		   measure("Msg size",size(Msg),
			   begin
			       K2 = setelement(1, K1, Key),
			       K3 = setelement(2, K2, IV),
			       K4 = setelement(3, K3, IV),
			       {Encrypted,_} = ocb128crypt:encrypt(K4,Msg),
			       {Decrypted,_} = ocb128crypt:decrypt(K4,Encrypted),
			       Msg == Decrypted
			   end
			  )
		  )
	  )
       )
     ).
