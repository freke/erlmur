-module(mock_mumble_handler).
-behaviour(mumble_server_behaviour).

start_link(_Opts) ->
    %% Simple mock - just return {ok, self()} as a placeholder pid
    {ok, self()}.

stop(Pid) when is_pid(Pid) ->
    catch exit(Pid, normal),
    ok.

init(_Opts) ->
    {ok, #{}}.

handle_msg(Msg, State) ->
    case maps:get(message_type, Msg) of
        'TextMessage' ->
            %% Reply with another text message
            Reply = #{
                message_type => 'TextMessage',
                message => <<"Echo: ", (maps:get(message, Msg))/binary>>
            },
            %% We need to know who to send it to, but mock doesn't have session pid easily.
            %% mumble_server_conn handles casting back if we return it? 
            %% Actually mumble_server_conn:handle_protocol_msg calls Mod:handle_msg.
            %% If we want to send something back, we'd typically use mumble_server_conn:send(self(), Reply),
            mumble_server_conn:send(self(), Reply),
            {ok, State};
        _ ->
            {ok, State}
    end.

get_caps(_State) ->
    #{major => 1, minor => 2, patch => 4,
      os => <<"MockOS">>,
      release => <<"1.0">>,
      os_version => <<"1.0">>}.

authenticate(_AuthMsg, State) ->
    UserInfo = #{session_id => 1, name => <<"MockUser">>},
    {ok, UserInfo, State}.
