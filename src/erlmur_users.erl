%%%-------------------------------------------------------------------
%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created :  1 Apr 2013 by David AAberg <davabe@hotmail.com>
%%%-------------------------------------------------------------------
-module(erlmur_users).

-export([init/0,count/1,list/1,remove/3,add/4,user/2]).

-include("mumble_pb.hrl").

-record(user,{lastid,users}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
init() ->
    #user{lastid=0,users=dict:new()}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
count(Users) ->
    dict:size(Users#user.users).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
list(#user{users=U}) ->
    dict:fetch_keys(U).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
user(Key,#user{users=U}) ->
    dict:fetch(Key,U).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
remove(Pid,Reason, State = #user{users=U}) ->
    case dict:find(Pid,U) of
	{ok,US} ->
	    error_logger:info_report([{erlmur_server,remove_user},
				      {session_id,US#userstate.session},
				      {reason,Reason}]),
	    UR=#userremove{ session=US#userstate.session, reason=Reason},
	    NU=dict:erase(Pid,U),
	    [erlmur_client:deluser(P,UR) || P <- dict:fetch_keys(NU)],
	    State#user{users=NU};
	_ ->
	    State
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
add(Pid,User,Session,#user{users=U,lastid=L}=State) ->
    UR=#userstate{name=User,user_id=L+1, session=Session},
    NU=dict:store(Pid, UR, U),
    error_logger:info_report([{erlmur_server,add_user},
			      {session_id,UR#userstate.session}]),
    [erlmur_client:newuser(P,UR) || P <- dict:fetch_keys(NU)],
    State#user{users=NU,lastid=L+1}.
