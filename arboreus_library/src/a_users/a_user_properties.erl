%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus users: a_user_properties data model handler
%%%
%%% @end
%%% Created : 06/17/2018 at 16:43
%%%-------------------------------------------------------------------
-module(a_user_properties).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants
-define(TABLE,?MODULE).

%% Data types
-include("../include/types/types_a_general.hrl").
-include("../include/types/types_a_time.hrl").
-include("../include/types/types_a_users.hrl").

%% Data models
-include("../include/records/records_a_users.hrl").

%% API
-export([
	test/0,
	create/1,
	read/1,
	delete/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_user_properties) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	User_id = "test_id",
	Wrong_user_id = "wrong_id",
	A_user_properties = #a_user_properties{
		id = User_id,
		created = a_time:current(timestamp),
		bd = 1,
		first_name = "test",
		last_name = "test"
	},
	{ok,A_user_id} = create(A_user_properties),
	{existed,A_user_id} = create(A_user_properties),
	io:format("DONE! User created: ~p~n",[A_user_id]),
	{ok,A_user_properties} = read(User_id),
	{norow,Wrong_user_id} = read(Wrong_user_id),
	io:format("DONE! User read: ~p~n",[A_user_id]),
	{ok,User_id} = delete(User_id),
	{norow,User_id} = delete(User_id),
	io:format("DONE! User deleted: ~p~n",[A_user_id]),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_user_properties) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.

%% ----------------------------
%% @doc Delete a_user record from db
-spec delete(Key) -> {ok,Key} | {norow,Key} | {error,_Reason}
	when
	Key :: a_user_id().

delete(Key) -> a_mnesia:transaction_delete(?TABLE,Key).


%% ----------------------------
%% @doc Read a_user record from db
-spec read(A_user_id) -> {norow,A_user_id} | {ok,a_user_properties()}
	when
	A_user_id :: a_user_id().

read(A_user_id) -> a_mnesia:dirty_read(?TABLE,A_user_id).


%% ----------------------------
%% @doc Create a_user_properties
-spec create(Record) -> {ok,Id} | {existed,Id} | {aborted,_Reason}
	when
	Record :: a_user_properties(),
	Id :: a_user_id().

create(Record) when is_record(Record,a_user_properties) ->
	case a_mnesia:transaction_create_unique(Record) of
		{atomic,ok} -> {ok,Record#a_user_properties.id};
		{atomic,existed} -> {existed,Record#a_user_properties.id};
		Result -> Result
	end.