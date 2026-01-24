-module(erlmur_sup).

-moduledoc("The main supervisor for the erlmur application.").

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    logger:info("[erlmur_sup] Starting user manager..."),
    UserManager = #{
        id => erlmur_user_manager,
        start => {erlmur_user_manager, start_link, []},
        restart => permanent,
        type => worker
    },
    logger:info("[erlmur_sup] User manager started"),
    {ok, {{one_for_one, 5, 10}, [UserManager]}}.
