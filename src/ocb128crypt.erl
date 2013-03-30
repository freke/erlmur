%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 21 Mar 2013 by  <davabe@hotmail.com>

-module(ocb128crypt).

-export([new_key/0, key/1, encrypt/2,decrypt/2,local/1,remote/5,client_nonce/2]).

-on_load(init/0).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
init() ->
    SoName = case code:priv_dir(?MODULE) of {error, bad_name} ->
		     case filelib:is_dir(filename:join(["..", "priv"])) of
			 true ->
			     filename:join(["..", "priv", "ocb128crypt_nif"]);
			 false ->
			     filename:join(["priv", "ocb128crypt_nif"])
		     end;
		 Dir ->
		     filename:join(Dir, "ocb128crypt_nif")
	     end,
    (catch erlang:load_nif(SoName, 0)).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
client_nonce(Div,{Key,_Div,Eiv,DecryptHistory,{Good,Late,Lost,Resync},Remote}) ->
    {Key,Div,Eiv,DecryptHistory,{Good,Late,Lost,Resync+1},Remote}.
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
new_key() ->
    {_Key = crypto:rand_bytes(16),
     _Div = crypto:rand_bytes(16),
     _Eiv = crypto:rand_bytes(16),
     _DecryptHistory = <<16#FF>>,
     
     {_Good = 0,
      _Late = 0,
      _Lost = 0,
      _Resync = 0},
     
     {_RemoteGood = 0,
      _RemoteLate = 0,
      _RemoteLost = 0,
      _RemoteResync = 0}}.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
key({Key,DIV,EIV,_,_,_}) ->
    {Key,DIV,EIV}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
local({_Key,_Div,_Eiv,_DecryptHistory,Locale,_Remote}) ->
    Locale.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
remote(Good,Late,Lost,Resync,Key) ->
    Remote = {Good,Late,Lost,Resync},
    setelement(6,Key,Remote).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
encrypt(_Key, _Source) ->
    exit(ocb128crypt_nif_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
decrypt(_Key, _Source) ->
    exit(ocb128crypt_nif_not_loaded).

