-module(erlmur_session_registry).

-moduledoc """
Manages the registration and lookup of UDP sessions.

This module provides a mechanism to associate incoming UDP packets with the correct
client session based on Ip address and port, and to clean up stale entries.
""".

-behaviour(gen_server).

-export([
    start_link/0,
    stop/0,
    register_ip/2,
    register_ip_port/3,
    lookup/2,
    cleanup/1
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    % ETS table: {Ip, Pid}
    session_ip,
    % ETS table: {Ip, Port} => Pid
    session_ip_port,
    % Pid => Ref
    monitors = #{}
}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

register_ip(Ip, Pid) ->
    gen_server:call(?MODULE, {register_ip, Ip, Pid}).

register_ip_port(Ip, Port, Pid) ->
    gen_server:call(?MODULE, {register_ip_port, Ip, Port, Pid}).

lookup(Ip, Port) ->
    gen_server:call(?MODULE, {lookup, Ip, Port}).

cleanup(Pid) ->
    gen_server:cast(?MODULE, {cleanup, Pid}).

%% gen_server callbacks
init([]) ->
    SessionIp = ets:new(erlmur_session_ip, [set, named_table, public]),
    SessionIpPort = ets:new(erlmur_session_ip_port, [set, named_table, public]),
    {ok, #state{session_ip = SessionIp, session_ip_port = SessionIpPort}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(
    {register_ip, Ip, Pid}, _From, State = #state{session_ip = SessionIp, monitors = Monitors}
) ->
    logger:debug("Register_ip ~p", [Ip]),
    ets:insert(SessionIp, {Ip, Pid}),
    Ref = erlang:monitor(process, Pid),
    {reply, ok, State#state{monitors = maps:put(Pid, Ref, Monitors)}};
handle_call(
    {register_ip_port, Ip, Port, Pid},
    _From,
    State = #state{session_ip = SessionIp, session_ip_port = SessionIpPort}
) ->
    logger:debug("Register port: ip ~p port ~p", [Ip, Port]),
    ets:match_delete(SessionIp, {Ip, Pid}),
    ets:insert(SessionIpPort, {{Ip, Port}, Pid}),
    {reply, ok, State};
handle_call(
    {lookup, Ip, Port},
    _From,
    State = #state{session_ip = SessionIp, session_ip_port = SessionIpPort}
) ->
    logger:debug("Lookup Ip ~p Port ~p", [Ip, Port]),
    case ets:lookup(SessionIpPort, {Ip, Port}) of
        [{_, Pid}] ->
            {reply, {ok, Pid}, State};
        [] ->
            % fallback: all matches by Ip
            logger:debug("Registered ip: ~p", [ets:tab2list(SessionIp)]),
            IpMatches = [Pid || {Ip2, Pid} <- ets:tab2list(SessionIp), Ip2 =:= Ip],
            logger:debug("Registered ip: ~p", [ets:tab2list(SessionIpPort)]),
            PortMatches = [Pid || {{Ip2, _Port}, Pid} <- ets:tab2list(SessionIpPort), Ip2 =:= Ip],
            logger:debug("Matches found ~p ~p", [IpMatches, PortMatches]),
            {reply, {ip_matches, IpMatches ++ PortMatches}, State}
    end.

handle_cast({cleanup, Pid}, State) ->
    NewState = do_cleanup(Pid, State),
    {noreply, NewState}.

handle_info({'DOWN', Ref, process, Pid, _Reason}, State = #state{monitors = Mon}) ->
    case maps:get(Pid, Mon, undefined) of
        Ref ->
            NewState = do_cleanup(Pid, State),
            {noreply, NewState#state{monitors = maps:remove(Pid, Mon)}};
        _ ->
            {noreply, State}
    end;
handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.

%% Internal
do_cleanup(Pid, State = #state{session_ip = SessionIp, session_ip_port = SessionIpPort}) ->
    ets:match_delete(SessionIp, {{'_', Pid}}),
    ets:match_delete(SessionIpPort, {{'_', '_'}, Pid}),
    State.
