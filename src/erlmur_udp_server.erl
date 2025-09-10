-module(erlmur_udp_server).

-moduledoc """
UDP server for handling encrypted client messages and version pings in the Erlmur system.
""".

-behaviour(gen_server).

%% API
-export([start_link/1, send/3]).
%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("erlmur.hrl").

-define(SERVER, ?MODULE).

-record(state, {socket}).

%%%===================================================================
%%% API
%%%===================================================================
%%%
-spec start_link(inet:port_number()) -> {ok, pid()} | {error, term()}.
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

-doc """
Sends a UDP datagram to the specified address and port via the internal UDP socket.
""".
-spec send(inet:ip_address(), inet:port_number(), binary()) -> ok.
send(Address, Port, Data) ->
    gen_server:cast(?SERVER, {send, Address, Port, Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([any()]) -> {ok, #state{}} | {stop, term()}.
init([Port]) ->
    process_flag(trap_exit, true),
    case gen_udp:open(Port, [binary]) of
        {ok, Socket} ->
            gen_udp:controlling_process(Socket, self()),
            {ok, #state{socket = Socket}};
        {error, Reason} ->
            logger:error("Failed to open UDP socket on port ~p: ~p", [Port, Reason]),
            {stop, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({send, Address, Port, Data}, State = #state{socket = Socket}) ->
    gen_udp:send(Socket, Address, Port, Data),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(
    {udp, _Socket, IP, PortNo, <<0:32, Timestamp:64>>},
    State = #state{socket = Socket}
) ->
    logger:debug("Received version ping from ~p:~p", [IP, PortNo]),
    %erlmur_server:usercount(),
    #{erlmur_ssl := #{active_connections := ClientCount}} = ranch:info(),
    Version = erlmur_server:version(),
    Config = erlmur_server:config(),
    MaxClients = Config#server_config.max_clients,
    MaxBandwidth = Config#server_config.max_bandwidth,
    gen_udp:send(
        Socket,
        IP,
        PortNo,
        <<
            (Version#version.major):16,
            (Version#version.minor):8,
            (Version#version.patch):8,
            Timestamp:64,
            ClientCount:32,
            MaxClients:32,
            MaxBandwidth:32
        >>
    ),
    {noreply, State};
handle_info({udp, _Socket, IP, PortNo, EncryptedMsg}, State) ->
    logger:debug("Received encrypted UDP packet from ~p:~p", [IP, PortNo]),
    case erlmur_session_registry:lookup(IP, PortNo) of
        {ok, Pid} ->
            Pid ! {udp, IP, PortNo, EncryptedMsg, false};
        {ip_matches, []} ->
            logger:debug("No session found for UDP packet from ~p:~p", [IP, PortNo]);
        {ip_matches, Pids} ->
            lists:foreach(
                fun(SessionPid) ->
                    SessionPid ! {udp, IP, PortNo, EncryptedMsg, true}
                end,
                Pids
            )
    end,
    {noreply, State};
handle_info(Info, State) ->
    logger:warning("Unhandled info: ~p", [Info]),
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    gen_udp:close(State#state.socket),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
