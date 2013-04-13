%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 27 Mar 2013 by  <davabe@hotmail.com>

-module(erlmur).

-export([start/0]).

start() ->
    ssl:start(),
    application:start(mnesia),
    application:start(erlmur).
