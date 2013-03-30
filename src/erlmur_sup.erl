%%%-------------------------------------------------------------------
%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 29 Mar 2013 by  <davabe@hotmail.com>
%%%-------------------------------------------------------------------
-module(erlmur_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

-define(DEF_PORT, 64738).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    ListenPort = application:get_env(application:get_application(), listen_port, ?DEF_PORT),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port]) ->
    ErlmurClientSup = ?CHILD(erlmur_client_sup,supervisor,[]),
    ErlmurSsl = ?CHILD(erlmur_ssl_server,worker,[Port]),
    ErlmurUdp = ?CHILD(erlmur_udp_server,worker,[Port]),
    ErlmurServer = ?CHILD(erlmur_server,worker,[]),

    {ok, { {one_for_one, 5, 10}, [ErlmurClientSup,ErlmurSsl,ErlmurUdp,ErlmurServer]} }.

