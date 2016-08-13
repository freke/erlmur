%%%-------------------------------------------------------------------
%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013,
%%% @doc
%%%
%%% @end
%%% Created :  1 Apr 2013 by David AAberg <davabe@hotmail.com>
%%%-------------------------------------------------------------------
-module(erlmur_users).

-export(
	[
		init/1,
		id/1,
		client_pid/1,
		session/1,
		name/1,
		channel_id/1,
		all_user_states/1,
		list_clients/0,
		find_user/1,
		fetch_user/1,
		remove/4,
		add/3,
		update/3,
		list/0,
		move_to_channel/3,
		send_to_all/1,
		list_registered_users/0,
		users_in_channel/1
	]).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("record_info/include/record_info.hrl").

-export_record_info([user]).

-record(user,{id,name,address,session,channel_id,client_pid,self_mute=false,self_deaf=false,comment}).
-record(counter_entry, {id, value=0}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
init(Nodes) ->
  mnesia:create_table(user,
		[
			{attributes, record_info(fields, user)},
			{ram_copies, Nodes},
			{index,[#user.client_pid]},
			{type, set}
		]),

  ets:new(user_counters, [set, {keypos, #counter_entry.id}, named_table, public]),
	ets:insert(user_counters, #counter_entry{id=userid, value=0}),
  ets:insert(user_counters, #counter_entry{id=sessionid, value=0}),
	ok = mnesia:wait_for_tables([user],5000).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
id(User) ->
    User#user.id.

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
name(User) ->
    User#user.name.

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
all_user_states(Actor) ->
  F = fun() ->
		mnesia:foldl(fun(User,Acc) -> [userstate(User,Actor)|Acc] end, [], user)
	end,
  mnesia:activity(transaction, F).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
list_clients() ->
  F = fun() ->
		mnesia:foldl(fun(User,Acc) -> [User#user.client_pid|Acc] end, [], user)
	end,
    mnesia:activity(transaction, F).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
find_user({address,Address}) ->
  Match = ets:fun2ms(fun(X = #user{address=A}) when Address =:= A -> X end),
  F = fun() ->
		mnesia:select(user, Match)
	end,
  mnesia:activity(transaction, F);
find_user({name,Name}) ->
    Match = ets:fun2ms(fun(X = #user{name=N}) when Name =:= N -> X end),
    F = fun() ->
		mnesia:select(user, Match)
	end,
    mnesia:activity(transaction, F);
find_user({channel_id,ChannelId}) ->
    Match = ets:fun2ms(fun(X = #user{channel_id=C}) when ChannelId =:= C -> X end),
    F = fun() ->
		mnesia:select(user, Match)
	end,
    mnesia:activity(transaction, F);
find_user({session,[]}) ->
    [];
find_user({session,[Session|Sessions]}) ->
    lists:flatten([find_user({session,Session}),find_user({session,Sessions})]);
find_user({session,Session}) ->
    Match = ets:fun2ms(fun(X = #user{session=S}) when Session =:= S -> X end),
    F = fun() ->
		mnesia:select(user, Match)
	end,
    mnesia:activity(transaction, F);
find_user({client_pid,Pid}) ->
    Match = ets:fun2ms(fun(X = #user{client_pid=P}) when Pid =:= P -> X end),
    F = fun() ->
		mnesia:select(user, Match)
	end,
    mnesia:activity(transaction, F).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
fetch_user({client_pid,Pid}) ->
  F = fun() ->
		mnesia:index_read(user, Pid, #user.client_pid)
	end,
  [U] = mnesia:activity(transaction, F),
  U;
fetch_user({session,Session}) ->
  Match = ets:fun2ms(fun(X = #user{session=S}) when Session =:= S -> X end),
  F = fun() ->
		mnesia:select(user, Match)
	end,
  [U] = mnesia:activity(transaction, F),
  U;
fetch_user({id,Id}) ->
  F = fun() ->
		mnesia:read(user, Id)
	end,
  [U] = mnesia:activity(transaction, F),
  U.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
remove([], _Actor, _Reason, _Ban) ->
  ok;
remove([User|Users], Actor, Reason, Ban) ->
  remove(User, Actor, Reason, Ban),
  remove(Users, Actor, Reason, Ban);
remove(User, Actor, Reason , Ban) ->
  F = fun() ->
		mnesia:delete({user,User#user.id})
	end,
  mnesia:activity(transaction, F),
  send_to_all(userremove(User,Actor,Reason,Ban)).

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
  F = fun() ->
		mnesia:write(User)
	end,
  mnesia:activity(transaction, F),
  error_logger:info_report([{erlmur_server,add_user},
		      {id,UserId},
		      {name,Name},
		      {session_id,SessionId}]),
  send_to_all(userstate(User,undefined)),
  UserId.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
update([],User,Actor) ->
  F = fun() ->
		mnesia:write(user, User, write)
	end,
  mnesia:activity(transaction, F),
  send_to_all(userstate(User,Actor));
update([{_,undefined}|R], User,Actor) ->
  update(R,User,Actor);
update([{channel_id,NewChannel}|R], User,Actor) ->
  update(R,User#user{channel_id=NewChannel},Actor);
update([{client_pid,NewClient}|R], User,Actor) ->
  update(R,User#user{client_pid=NewClient},Actor);
update([{name,NewName}|R], User,Actor) ->
  update(R,User#user{name=NewName},Actor);
update([{self_mute,Mute}|R], User,Actor) ->
  update(R,User#user{self_mute=Mute},Actor);
update([{self_deaf,Deaf}|R], User,Actor) ->
  update(R,User#user{self_deaf=Deaf},Actor);
update([{comment,Comment}|R], User,Actor) ->
  update(R,User#user{comment=Comment},Actor);
update([V|R],User,Actor) ->
  error_logger:info_report([{erlmur_users,update},{not_updating,V}]),
  update(R,User,Actor).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
list() ->
  F = fun() ->
		mnesia:foldl(fun(User,Acc) -> [User|Acc] end, [], user)
	end,
  mnesia:activity(transaction, F).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
move_to_channel([],_,_) ->
  ok;
move_to_channel([User|Users],ChannelId,Actor) ->
  move_to_channel(User,ChannelId,Actor),
  move_to_channel(Users,ChannelId,Actor);
move_to_channel(User,ChannelId,Actor) ->
  error_logger:info_report([{erlmur_users,move_to_channel},{user,User},{channel,ChannelId}]),
  F = fun() ->
		[U] = mnesia:dirty_read(user,User#user.id),
		NewU = U#user{channel_id=ChannelId},
		mnesia:write(NewU),
		send_to_all(userstate(NewU,Actor))
	end,
  mnesia:activity(transaction, F).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
send_to_all(Msg) ->
  F = fun() ->
		mnesia:foldl(fun(#user{client_pid=P},_) ->
				     P ! Msg
			     end,
			     [],
			     user)
	end,
  mnesia:activity(transaction, F).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
list_registered_users() ->
    [].

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
users_in_channel([]) ->
  [];
users_in_channel([Channel|Channels]) ->
  lists:append(users_in_channel(Channel),users_in_channel(Channels));
users_in_channel(Channel) ->
  ChannelId = erlmur_channels:channel_id(Channel),
  Match = ets:fun2ms(fun(X = #user{channel_id=C}) when ChannelId =:= C -> X end),
  F = fun() ->
		mnesia:select(user, Match)
	end,
  mnesia:activity(transaction, F).

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
userstate(User,undefined) ->
  {userstate,record_info:record_to_proplist(User, ?MODULE)};
userstate(User,Actor) ->
  ActorId = id(Actor),
  {userstate,[{actor,ActorId}|record_info:record_to_proplist(User, ?MODULE)]}.

userremove(User,undefined,Reason,Ban) ->
  {userremove,[{reason,Reason},{ban,Ban}|record_info:record_to_proplist(User, ?MODULE)]};
userremove(User,Actor,Reason,Ban) ->
  {userremove,[{reason,Reason},{actor,Actor},{ban,Ban}|record_info:record_to_proplist(User, ?MODULE)]}.
