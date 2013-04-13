%%%-------------------------------------------------------------------
%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 29 Mar 2013 by  <davabe@hotmail.com>
%%%-------------------------------------------------------------------
-module(erlmur_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([start_client/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    mnesia:wait_for_tables([users], 5000),
    erlmur_sup:start_link().

stop(_State) ->
    ok.

start_client() ->
    supervisor:start_child(erlmur_client_sup, []).
