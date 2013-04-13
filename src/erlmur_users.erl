%%%-------------------------------------------------------------------
%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created :  1 Apr 2013 by David AAberg <davabe@hotmail.com>
%%%-------------------------------------------------------------------
-module(erlmur_users).

-export([init/1,
	 client_pid/1,
	 session/1,
	 name/1,
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

-record(user,{id,name,address,session,channel_id,client_pid}).
-record(counter_entry, {id, value=0}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
init(Nodes) ->
    mnesia:create_table(user,
			[{attributes, record_info(fields, user)},
			 %{index, [#user.id]},
			 {ram_copies, Nodes},
			 {type, set}]),

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
count() ->
    F = fun() ->
		mnesia:all_keys(user)
	end,
    length(mnesia:activity(transaction, F)).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
all_user_states() ->
    F = fun() ->
		mnesia:foldl(fun(User,Acc) -> [userstate(User)|Acc] end, [], user)
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
    mnesia:activity(transaction, F).		

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
fetch_user({client_pid,Pid}) ->
    Match = ets:fun2ms(fun(X = #user{client_pid=C}) when C =:= Pid -> X end),
    F = fun() ->
		mnesia:select(user, Match)
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
remove([], _Reason) ->
    ok;
remove([User|User], Reason) ->
    remove(User,Reason),
    remove(User,Reason);
remove(User, Reason) ->
    F = fun() ->
		mnesia:delete({user,User#user.id})
	end,
    mnesia:activity(transaction, F),
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
    F = fun() ->
		
		mnesia:write(User)
	end,
    mnesia:activity(transaction, F),
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
    F = fun() ->
		mneia:write(user, User, write)
	end,
    mnesia:activity(transaction, F),
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
    F = fun() ->
		mnesia:foldl(fun(User,Acc) -> [User|Acc] end, [], user)
	end,
    mnesia:activity(transaction, F).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
in_channel(ChannelId) ->
    Match = ets:fun2ms(fun(X = #user{channel_id=C}) when ChannelId =:= C -> X end),
    F = fun() ->
		mnesia:select(user, Match)
	end,
    mnesia:activity(transaction, F).

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
    F = fun() ->
		U = fetch_user({id, User#user.id}),
		mnesia:write(U#user{channel_id=ChannelId}),
		send_to_all(userstate(U))
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
%% Internal
%%--------------------------------------------------------------------
userstate(User) ->
    erlmur_message:userstate(record_info:record_to_proplist(User, ?MODULE)).

userremove(User,Reason) ->
    erlmur_message:userremove([{reason,Reason}|record_info:record_to_proplist(User, ?MODULE)]).
