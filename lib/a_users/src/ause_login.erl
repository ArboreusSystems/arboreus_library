%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus users: ause_login data model handler
%%%
%%% @end
%%% Created : 06/15/2018 at 20:50
%%%-------------------------------------------------------------------
-module(ause_login).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants
-define(TABLE,?MODULE).

%% Data types
-include("../data_models/types/types_general.hrl").
-include("../data_models/types/types_time.hrl").
-include("../data_models/types/types_a_users.hrl").

%% Data models
-include("../data_models/records/records_a_users.hrl").

%% API
-export([
	test/0,
	create/1,
	read/1,
	delete/1,
	select/3
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (~p) testing started at:~n~p (~p)~n",
		[?MODULE,a_time:from_timestamp(rfc850,Time_start),Time_start]
	),
	
	User = "test_user",
	Wrong_user = "wrong_user",
	Login1 = "login1",
	Login2 = "login2",
	Wrong_login = "wrong_login",
	Kind = "test",
	Ause_login1 = #ause_login{login = Login1,user = User,kind = Kind},
	Ause_login2 = #ause_login{login = Login2,user = User,kind = Kind},
	Ause_login_kind = #ause_login_kind{id = Kind},
	{nokind,Login1} = create(Ause_login1),
	ok = mnesia:dirty_write(Ause_login_kind),
	{ok,Login1} = create(Ause_login1),
	{existed,Login1} = create(Ause_login1),
	{ok,Login2} = create(Ause_login2),
	io:format("DONE! Logins created: ~p, ~p~n",[Login1,Login2]),
	{ok,Ause_login1} = read(Login1),
	{ok,Ause_login2} = read(Login2),
	{norow,Wrong_login} = read(Wrong_login),
	io:format("DONE! Logins reading passed: ~p, ~p~n",[Login1,Login2]),
	{ok,[Ause_login1,Ause_login2]} = select(by_user,[User],return_records),
	{norow,[Wrong_user]} = select(by_user,[Wrong_user],return_records),
	io:format("DONE! Logins selection passed: ~p, ~p~n",[Login1,Login2]),
	{ok,Login1} = delete(Login1),
	{norow,Login1} = delete(Login1),
	{last,Login2} = delete(Login2),
	io:format("DONE! Logins deleting passed: ~p, ~p~n",[Login1,Login2]),
	ok = mnesia:dirty_delete(?TABLE,Login2),
	ok = mnesia:dirty_delete(element(1,Ause_login_kind),Kind),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (~p) testing finished at:~n~p (~p)~n",
		[?MODULE,a_time:from_timestamp(rfc850,Time_stop),Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Select ause-logins by parameters
-spec select(Kind,Properties,Return_mode) ->
	{ok,_Datum} | {norow,Properties} | {aborted,_Reason}
	when
	Kind :: by_user,
	Properties :: list_of_properties(),
	Return_mode :: logins | return_records | return_record.

select(run,[Properties,Match_head,Guard],Return_mode) ->
	case mnesia:dirty_select(
		?TABLE,[{Match_head,Guard,['$_']}]
	) of
		[] -> {norow,Properties};
		{aborted,Reason} -> {aborted,Reason};
		Records -> select_return(Return_mode,Records)
	end;
select(by_user,[User],Return_mode) ->
	Match_head = #ause_login{user = '$1',_ = '_'},
	Guard = [{'==','$1',User}],
	select(run,[[User],Match_head,Guard],Return_mode).


%% ----------------------------
%% @doc Select datum from record by defined field, additional for select/3
-spec select_return(Datum,Return_mode) -> {ok,_Datum}
	when
	Datum :: ause_login() | list_of_records(),
	Return_mode :: logins | return_records | return_record.

select_return(logins,Records) ->
	{ok,[{Record#ause_login.login,Record#ause_login.kind} || Record <- Records]};
select_return(_,Datum) -> {ok,Datum}.


%% ----------------------------
%% @doc Delete ause_login by login
-spec delete(Login) -> {ok,Login} | {last,Login} | {norow,Login} | {aborted,_Reason}
	when
	Login :: a_user_login().

delete(Login) ->
	case mnesia:transaction(fun() ->
		case mnesia:read(?TABLE,Login) of
			[Ause_login] ->
				case mnesia:select(?TABLE,[{
					#ause_login{user = '$1',_ = '_'},
					[{'==','$1',Ause_login#ause_login.user}],
					['$_']
				}]) of
					[Ause_login] -> last;
					_ -> mnesia:delete_object(Ause_login)
				end;
			[] -> norow
		end
	end) of
		{atomic,norow} -> {norow,Login};
		{atomic,last} -> {last,Login};
		{atomic,ok} -> {ok,Login};
		Result -> Result
	end.


%% ----------------------------
%% @doc Read ause_login
-spec read(Login) -> {norow,Login} | {ok,ause_login()}
	when
	Login :: a_user_login().

read(Login) -> a_mnesia:dirty_read(?TABLE,Login).


%% ----------------------------
%% @doc Create new ause_login
-spec create(Record) -> {nokind,Login} | {ok,Login} | {existed,Login} | {aborted,_Reason}
	when
	Record :: ause_login(),
	Login :: a_user_login().

create(Record) when is_record(Record,ause_login) ->
	case ause_login_kind:read(Record#ause_login.kind) of
		{ok,_} ->
			case a_mnesia:transaction_create_unique(Record) of
				{atomic,ok} -> {ok,Record#ause_login.login};
				{atomic,existed} -> {existed,Record#ause_login.login};
				Result -> Result
			end;
		_ -> {nokind,Record#ause_login.login}
	end.