-module(mumble_udp_server).
-moduledoc """
UDP server for handling Mumble voice traffic.

This gen_server opens a UDP socket on port 64738 and routes incoming
encrypted voice packets to the appropriate session process.
""".

-behaviour(gen_server).

-include("mumble_protocol.hrl").
-include("MumbleUDP_gpb.hrl").

%% API
-export([start_link/1, send/3, get_port/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).


-record(state, {
    socket :: gen_udp:socket(),
    port :: inet:port_number()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

-doc """
Send a UDP packet to a specific client address.
Input: IP address, port number, and binary data.
Output: ok on success, {error, term()} on failure.
""".
-spec send(IP :: inet:ip_address(), inet:port_number(), binary()) -> ok | {error, term()}.
send(IP, Port, Data) ->
    gen_server:cast(?MODULE, {send, IP, Port, Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Port]) ->
    logger:debug("[mumble_udp_server] Opening UDP socket on port ~p...", [Port]),
    case gen_udp:open(Port, [binary, {active, true}]) of
        {ok, Socket} ->
            {ok, ActualPort} = inet:port(Socket),
            logger:notice("[mumble_udp_server] UDP server listening on port ~p", [ActualPort]),
            {ok, #state{socket = Socket, port = ActualPort}};
        {error, Reason} ->
            logger:error("[mumble_udp_server] Failed to open UDP socket on port ~p: ~p", [Port, Reason]),
            {stop, Reason}
    end.

handle_call(get_port, _From, State = #state{port = Port}) ->
    {reply, {ok, Port}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({send, IP, Port, Data}, State = #state{socket = Socket}) ->
    case gen_udp:send(Socket, IP, Port, Data) of
        ok -> ok;
        {error, Reason} ->
            logger:warning("Failed to send UDP packet to ~p:~p: ~p", [IP, Port, Reason])
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Legacy ping format (12 bytes): 4 bytes type (0) + 8 bytes timestamp
handle_info(
    {udp, _Socket, IP, PortNo, <<0:32, Timestamp:64>>},
    State = #state{socket = Socket}
) ->
    logger:debug("[mumble_udp_server] Received legacy ping from ~p:~p", [IP, PortNo]),
    Listeners = ranch:info(),
    ClientCount = get_active_connections(Listeners),
    Version = mumble:server_version(),
    Config = mumble:serverconfig(),
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
%% Protobuf Ping message (MumbleUDP 1.5+)
handle_info(
    {udp, _Socket, IP, PortNo, <<1:8, _/binary>> = Data},
    State = #state{socket = Socket}
) ->
    logger:debug("[mumble_udp_server] Received protobuf ping from ~p:~p", [IP, PortNo]),
    try 'MumbleUDP_gpb':decode_msg(Data, 'Ping') of
        #'Ping'{timestamp = Timestamp, request_extended_information = RequestExtended} ->
            Response = case RequestExtended of
                true ->
                    Listeners = ranch:info(),
                    ClientCount = get_active_connections(Listeners),
                    Version = mumble:server_version(),
                    Config = mumble:serverconfig(),
                    #'Ping'{
                        timestamp = Timestamp,
                        request_extended_information = false,
                        %% TODO: Implement proper version_v2 encoding (see Mumble issue #5827)
                        %% Currently using legacy version format (major * 2^16 + minor * 2^8 + patch)
                        server_version_v2 = Version#version.major * 65536 +
                                           Version#version.minor * 256 +
                                           Version#version.patch,
                        user_count = ClientCount,
                        max_user_count = Config#server_config.max_clients,
                        max_bandwidth_per_user = Config#server_config.max_bandwidth
                    };
                false ->
                    #'Ping'{
                        timestamp = Timestamp,
                        request_extended_information = false
                    }
            end,
            ResponseBin = 'MumbleUDP_gpb':encode_msg(Response),
            gen_udp:send(Socket, IP, PortNo, ResponseBin),
            {noreply, State}
    catch
        _:Reason ->
            logger:debug("[mumble_udp_server] Failed to decode protobuf ping from ~p:~p: ~p", [IP, PortNo, Reason]),
            {noreply, State}
    end;
handle_info({udp, _Socket, IP, Port, Data}, State) ->
		logger:debug("UDP Message received"),
    Addr = {IP, Port},
    case erlmur_user_manager:get_session_by_udp(Addr) of
        {ok, Pid} ->
            %% Route to existing session
            mumble_server_conn:udp_packet(Pid, Data, Addr);
        {error, not_found} ->
            %% First UDP packet from this address - try all sessions
            %% Each session will try to decrypt and claim the address if successful
            AllUsers = erlmur_user_manager:get_all_users(),
            lists:foreach(
                fun(User) ->
                    Pid = element(3, User),  %% #user.pid is the 3rd field
                    mumble_server_conn:udp_packet(Pid, Data, Addr)
                end,
                AllUsers
            )
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
    gen_udp:close(Socket),
    ok.

-spec get_port() -> inet:port_number().
get_port() ->
    gen_server:call(?MODULE, get_port).

get_active_connections(Listeners) ->
    maps:fold(
        fun(_, #{active_connections := N}, Acc) -> Acc + N;
           (_, _, Acc) -> Acc
        end,
        0,
        Listeners
    ).
