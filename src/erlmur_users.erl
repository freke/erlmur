%%%-------------------------------------------------------------------
%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created :  1 Apr 2013 by David AAberg <davabe@hotmail.com>
%%%-------------------------------------------------------------------
-module(erlmur_users).

-export([init/0,
	 client_pid/1,
	 session/1,
	 channel_id/1,
	 count/0,
	 all_user_states/0,
	 list_clients/0,
	 find_user/1,
	 fetch_user/1,
	 remove/2,
	 add/3,
	 update/1,
	 list/0,
	 in_channel/1,
	 move_to_channel/2,
	 send_to_all/1]).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("record_info/include/record_info.hrl").

-export_record_info([user]).

-record(user,{name,id,address,session,channel_id,client_pid}).
-record(counter_entry, {id, value=0}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
init() ->
    ets:new(users, [set, {keypos,#user.id},named_table, public]),
    ets:new(user_counters, [set, {keypos, #counter_entry.id}, named_table, public]),
    ets:insert(user_counters, #counter_entry{id=userid, value=0}),
    ets:insert(user_counters, #counter_entry{id=sessionid, value=0}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
client_pid(User) ->
    User#user.client_pid.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
session(User) ->
    User#user.session.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
channel_id(User) ->
    User#user.channel_id.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
count() ->
    proplists:get_value(size,ets:info(users)).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
all_user_states() ->
    ets:foldl(fun(User,Acc) -> [userstate(User)|Acc] end, [], users).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
list_clients() ->
    ets:foldl(fun(User,Acc) -> [User#user.client_pid|Acc] end, [], users).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
find_user({address,Address}) ->
    Match = ets:fun2ms(fun(X = #user{address=A}) when Address =:= A -> X end),
    ets:select(users, Match).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
fetch_user({client_pid,Pid}) ->
    Match = ets:fun2ms(fun(X = #user{client_pid=C}) when C =:= Pid -> X end),
    [U] = ets:select(users, Match),
    U;
fetch_user({session,Session}) ->
    Match = ets:fun2ms(fun(X = #user{session=S}) when Session =:= S -> X end),
    [U] = ets:select(users, Match),
    U;
fetch_user({id,Id}) ->
    [U] = ets:lookup(users, Id),
    U.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
remove([], _Reason) ->
    ok;
remove([User|Users], Reason) ->
    remove(User,Reason),
    remove(Users,Reason);
remove(User, Reason) ->
    ets:delete(users,User#user.id),
    send_to_all(userremove(User,Reason)).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
add(Pid,Name,Address) ->
    UserId = ets:update_counter(user_counters, userid, {#counter_entry.value, 1}),
    SessionId = ets:update_counter(user_counters, sessionid, {#counter_entry.value, 1}),
    User=#user{name=Name,
	       id=UserId,
	       session=SessionId,
	       address=Address,
	       client_pid=Pid,
	       channel_id=0},
    ets:insert(users,User),
    error_logger:info_report([{erlmur_server,add_user},
			      {name,Name},
			      {session_id,SessionId}]),
    send_to_all(userstate(User)),
    SessionId.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
update(User) ->
    U = fetch_user({session,proplists:get_value(session,User)}),
    NewUser = update(User,U),
    send_to_all(userstate(NewUser)),
    NewUser.

update([],User) ->
    ets:insert(users, User),
    User;
update([{_,undefined}|R], User) ->
    update(R,User);
update([{channel_id,NewChannel}|R], User) ->
    update(R,User#user{channel_id=NewChannel});
update([{client_pid,NewClient}|R], User) ->
    update(R,User#user{client_pid=NewClient});
update([{name,NewName}|R], User) ->
    update(R,User#user{name=NewName});
update([V|R],User) ->
    error_logger:info_report([{erlmur_users,update},{not_updating,V}]),
    update(R,User).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
list() ->
    ets:foldl(fun(User,Acc) -> [User|Acc] end, [], users).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
in_channel(ChannelId) ->
    Match = ets:fun2ms(fun(X = #user{channel_id=C}) when ChannelId =:= C -> X end),
    ets:select(users, Match).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
move_to_channel([],_) ->
    ok;
move_to_channel([User|Users],ChannelId) ->
    move_to_channel(User,ChannelId),
    move_to_channel(Users,ChannelId);
move_to_channel(User,ChannelId) ->
    ets:update_element(users, User#user.id, {#user.channel_id,ChannelId}),
    [U] = ets:lookup(users, User#user.id),
    send_to_all(userstate(U)).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
send_to_all(Msg) ->
    ets:foldl(fun(#user{client_pid=P},_) -> 
		      P ! Msg
	      end, 
	      [], 
	      users).

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
userstate(User) ->
    erlmur_message:userstate(record_info:record_to_proplist(User, ?MODULE)).

userremove(User,Reason) ->
    erlmur_message:userremove([{reason,Reason}|record_info:record_to_proplist(User, ?MODULE)]).
