-module(mock_transport).

-moduledoc """
Mock SSL/TCP transport and Ranch for testing connection modules.

This module provides a mock transport layer that can be used to test
mumble_server_conn and mumble_client_conn without requiring actual network
connections. It simulates SSL socket behavior and allows tests to inject
messages and verify sent data.
""".

-behaviour(gen_server).
-behaviour(ranch_transport).

%% Public API
-export([start_link/0, send_message/2, recv_message/1]).
-export([messages/1, active_n/2]).
-export([setup_ranch_mock/0, cleanup_ranch_mock/0]).

%% Ranch transport callbacks
-export([name/0, secure/0, listen/1, accept/2, accept_ack/2, handshake/1, handshake/2, connect/3, connect/4]).
-export([recv/3, recv_proxy/5, send/2, sendfile/2, sendfile/4, sendfile/5, setopts/2, getopts/2]).
-export([getstat/1, getstat/2, controlling_process/2, peername/1, sockname/1, shutdown/2, close/1]).
-export([cleanup/1, messages/0, recv_proxy_header/2, recv_proxy_header/5, handshake_continue/2, handshake_continue/3, handshake_cancel/1, handshake/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(mock_socket, {
    pid :: pid()
}).

%% Public API for tests

-doc """
Start a new mock transport process for testing.

Returns {ok, Pid} where Pid can be used to send/receive messages.
""".
start_link() ->
    %% Need to make sure this process can receive gen_server calls
    gen_server:start_link(?MODULE, [], []).

-doc """
Send a message to the mock socket as if it came from the network.
Input: Mock socket PID and binary data.
""".
send_message(Pid, Data) when is_binary(Data) ->
    gen_server:cast(Pid, {inject_message, Data}).

-doc """
Receive messages that were sent through the mock socket.
Input: Mock socket PID.
Returns: List of binaries that were sent.
""".
recv_message(Pid) ->
    gen_server:call(Pid, get_sent_messages).

%% Ranch transport API

name() -> mock_transport.
secure() -> true.

listen(Opts) ->
    %% Mock implementation - just return a fake listen socket
    {ok, {mock_listen, Opts}}.

accept(_Timeout, ListenSocket) ->
    %% Mock implementation
    {ok, {mock_accepted, ListenSocket}}.

handshake(_Socket) ->
    handshake(_Socket, #{}).

handshake(_Socket, _Opts) ->
    %% Create a new mock socket process
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    {ok, #mock_socket{pid = Pid}}.

connect(Host, Port, Opts) ->
    connect(Host, Port, Opts, infinity).

connect(_Host, _Port, _Opts, _Timeout) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    {ok, #mock_socket{pid = Pid}}.

recv(#mock_socket{pid = Pid}, Length, Timeout) ->
    gen_server:call(Pid, {recv, Length, Timeout}, Timeout).

recv_proxy(_, _, _, _, _) ->
    ok.

send(#mock_socket{pid = Pid}, Data) ->
    gen_server:call(Pid, {send, Data}).

sendfile(Socket, Filename) ->
    sendfile(Socket, Filename, 0, 0, []).

sendfile(_Socket, _Filename, _Offset, _Bytes, _Opts) ->
    {ok, 0}.

setopts(#mock_socket{pid = Pid}, Opts) ->
    gen_server:call(Pid, {setopts, Opts}).

getopts(#mock_socket{pid = Pid}, Opts) ->
    gen_server:call(Pid, {getopts, Opts}).

getstat(_Socket) ->
    {ok, []}.

getstat(_Socket, _Items) ->
    {ok, []}.

controlling_process(#mock_socket{pid = Pid}, NewOwner) ->
    gen_server:call(Pid, {controlling_process, NewOwner}).

peername(#mock_socket{}) ->
    {ok, {{127, 0, 0, 1}, 12345}}.

sockname(#mock_socket{}) ->
    {ok, {{127, 0, 0, 1}, 64738}}.

shutdown(#mock_socket{pid = Pid}, How) ->
    gen_server:call(Pid, {shutdown, How}).

close(#mock_socket{pid = Pid}) ->
    gen_server:stop(Pid).

messages(#mock_socket{pid = Pid}) ->
    gen_server:call(Pid, get_messages).

sendfile(Socket, Filename, Offset, Bytes) ->
    sendfile(Socket, Filename, Offset, Bytes, []).

active_n(Socket, N) ->
    setopts(Socket, [{active, N}]).

%% Ranch mock functions

setup_ranch_mock() ->
    %% Store the original ranch module code
    meck:new(ranch, [unstick, passthrough]),
    meck:expect(ranch, handshake, fun mock_handshake/1),
    ok.

cleanup_ranch_mock() ->
    catch meck:unload(ranch),
    ok.

mock_handshake(_Ref) ->
    %% Create a mock socket for this reference
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    {ok, #mock_socket{pid = Pid}}.

cleanup(_) -> ok.
messages() -> {ok, {ssl, ssl_closed}}.
recv_proxy_header(_, _, _, _, _) -> {ok, {mock_proxy, []}}.
handshake_cancel(_) -> ok.
handshake(_, _, _) -> ok.
recv_proxy_header(_, _) -> {ok, {mock_proxy, []}}.
handshake_continue(_, _) -> ok.
handshake_continue(_, _, _) -> ok.
accept_ack(_, _) -> ok.

%% gen_server callbacks

init([]) ->
    {ok, #{
        owner => undefined,
        sent_messages => [],
        received_messages => [],
        opts => [],
        message_queue => []
    }}.

handle_call({recv, _Length, _Timeout}, _From, State = #{message_queue := [Msg | Rest]}) ->
    {reply, {ok, Msg}, State#{message_queue := Rest}};
handle_call({recv, _Length, Timeout}, _From, State) ->
    %% Wait for a message to be injected
    receive
        {inject_message, Data} ->
            {reply, {ok, Data}, State}
    after Timeout ->
        {reply, {error, timeout}, State}
    end;

handle_call({send, Data}, _From, State = #{sent_messages := Msgs}) ->
    {reply, ok, State#{sent_messages := [Data | Msgs]}};

handle_call({setopts, Opts}, _From, State = #{owner := Owner}) ->
    %% Handle active mode
    case lists:keyfind(active, 1, Opts) of
        {active, true} when Owner =/= undefined ->
            Owner ! {ssl_passive, self()},  %% Simulate ssl_passive first
            Owner ! {ssl, self(), <<>>};    %% Then active message
        {active, once} when Owner =/= undefined ->
            %% In real SSL, this triggers a message if data is available
            ok;
        _ ->
            ok
    end,
    {reply, ok, State#{opts := Opts}};

handle_call({getopts, Opts}, _From, State = #{opts := CurrentOpts}) ->
    Result = [{Opt, proplists:get_value(Opt, CurrentOpts)} || Opt <- Opts],
    {reply, {ok, Result}, State};

handle_call({controlling_process, NewOwner}, _From, State) ->
    {reply, ok, State#{owner := NewOwner}};

handle_call({shutdown, _How}, _From, State) ->
    {reply, ok, State};

handle_call(get_sent_messages, _From, State = #{sent_messages := Msgs}) ->
    {reply, lists:reverse(Msgs), State#{sent_messages := []}};

handle_call(get_messages, _From, State = #{message_queue := Queue}) ->
    {reply, Queue, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({inject_message, Data}, State = #{owner := Owner, opts := Opts}) ->
    NewQueue = case State of
        #{message_queue := Queue} -> Queue ++ [Data];
        _ -> [Data]
    end,
    
    %% Check if we should notify the owner (active mode)
    case lists:keyfind(active, 1, Opts) of
        {active, true} when Owner =/= undefined ->
            Owner ! {ssl, self(), Data};
        {active, once} when Owner =/= undefined ->
            Owner ! {ssl, self(), Data};
        _ ->
            ok
    end,
    
    {noreply, State#{message_queue := NewQueue}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
