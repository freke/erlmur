%%%-------------------------------------------------------------------
%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 17 Mar 2013 by  <davabe@hotmail.com>
%%%-------------------------------------------------------------------
-module(erlmur_ssl_server).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("public_key/include/public_key.hrl").

-define(SERVER, ?MODULE). 
-define(SSL_OPTIONS, [binary,
		      {active, false}, 
		      {reuseaddr, true},
		      {verify, verify_peer},
		      {verify_fun, {fun verify_peer/3, []}},
		      {fail_if_no_peer_cert, true}]).

-record(state, {listener}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(integer(),string(),string()) -> ignore | {error,_} | {ok,pid()}.
start_link(Port,ServerPem,KeyPem) when is_integer(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port,ServerPem,KeyPem], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init([any()]) -> {ok,#state{}} | {stop,_}.
init([Port,ServerPem,KeyPem]) ->
    error_logger:info_report([{erlmur_ssl_server,init},{server,ServerPem},{key,KeyPem}]),
    true = filelib:is_regular(KeyPem),
    true = filelib:is_regular(ServerPem),

    case ssl:listen(Port, [{certfile, ServerPem},
			   {keyfile, KeyPem}
			   |?SSL_OPTIONS ]) of
	{ok, Listen_socket} ->
	    gen_server:cast(self(), {accepted, self()}),
	    {ok, #state{listener = Listen_socket}};
	{error, Reason} ->
	    {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_call(_,_,#state{}) -> {reply,ok,#state{}}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(_,#state{}) -> {noreply,#state{}}.
handle_cast({accepted, _Pid}, State = #state{}) ->
    {noreply, wait_for_new_client(State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    ssl:close(State#state.listener),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
wait_for_new_client(#state{listener = Socket} = State) ->
    error_logger:info_msg("Waiting for connections...~n"),
    {ok, SslSocket} = ssl:transport_accept(Socket),
    gen_server:cast(self(), {accepted, self()}),
    {ok, Pid} = erlmur_app:start_client(),
    gen_server:cast(Pid, {socket, SslSocket}),
    State.

verify_peer(_OtpCert, {bad_cert, selfsigned_peer}, UserState) ->
    {valid, UserState};
verify_peer(_OtpCert, {bad_cert, _} = Reason, _UserState) ->
    {fail, Reason};
verify_peer(_OtpCert, {extension, _}, UserState) ->
    {unknown, UserState};
verify_peer(_OtpCert, valid, UserState) ->
    {valid, UserState};
verify_peer(_OtpCert, valid_peer, UserState) ->
    {valid, UserState}.
