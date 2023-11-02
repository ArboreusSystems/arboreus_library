%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus users: ause_login_kind data model handler
%%%
%%% @end
%%% Created : 06/15/2018 at 20:50
%%%-------------------------------------------------------------------
-module(ause_login_kind).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants
-define(TABLE,?MODULE).
-define(ID_DISCTIONARY,[numeric,alpha_lower,alpha_upper]).

%% Data types
-include("../include/types/types_general.hrl").
-include("../include/types/types_time.hrl").
-include("../include/types/types_a_users.hrl").

%% Data models
-include("../include/records/records_a_users.hrl").

%% API
-export([
	test/0,
	create/1,
	read/1,
	update/2,update_description/2,update_rule/2
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
	Time_stop = a_time:current(timestamp),
	Login_kind_description = "test",
	Login_kind_rule = "test",
	Login_kind_id = "test",
	Login_kind = #ause_login_kind{
		id = Login_kind_id,
		description = Login_kind_description,
		rule = Login_kind_rule
	},
	{ok,Login_kind_id} = create(Login_kind),
	{existed,Login_kind_id} = create(Login_kind),
	io:format("DONE! Login kind created: ~p~n",[Login_kind_id]),
	Wrong_login_kind_id = "wrong",
	{norow,Wrong_login_kind_id} = read(Wrong_login_kind_id),
	{ok,Login_kind} = read(Login_kind_id),
	io:format("DONE! Login kind reading: ~p~n",[Login_kind_id]),
	New_rule = "new_rule",
	New_description = "new_description",
	{ok,Login_kind_id} = update_rule(New_rule,Login_kind_id),
	{ok,Login_kind_id} = update_description(New_description,Login_kind_id),
	{norow,Wrong_login_kind_id} = update_description(New_description,Wrong_login_kind_id),
	{ok,#ause_login_kind{
		id = Login_kind_id,
		description = New_description,
		rule = New_rule
	}} = read(Login_kind_id),
	io:format("DONE! Login kind updating: ~p~n",[Login_kind_id]),
	ok = mnesia:dirty_delete(?TABLE,Login_kind_id),
	io:format("*** -------------------~n"),
	io:format(
		"Module (~p) testing finished at:~n~p (~p)~n",
		[?MODULE,a_time:from_timestamp(rfc850,Time_stop),Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Update rule for ause_login_kind
-spec update_rule(Rule,Id) ->
	{atomic,{ok,Id}} | {atomic,{norow,Id}} | {aborted,_Reason}
	when
	Rule :: ause_login_rule(),
	Id :: ause_login_kind_id().

update_rule(Rule,Id) ->
	update([{rule,Rule}],Id).


%% ----------------------------
%% @doc Update description for ause_login_kind
-spec update_description(Description,Id) -> {ok,Id} | {norow,Id} | {aborted,_Reason}
	when
	Description :: ause_login_description(),
	Id :: ause_login_kind_id().

update_description(Description,Id) ->
	update([{description,Description}],Id).


%% ----------------------------
%% @doc Update ause_login_kind
-spec update(New_values,Id) -> {ok,Id} | {norow,Id} | {aborted,_Reason}
	when
	New_values :: proplists:proplist(),
	Id :: ause_login_kind_id().

update(New_values,Id) ->
	case mnesia:transaction(fun() ->
		case mnesia:read(?TABLE,Id) of
			[] -> {norow,Id};
			[Old_record] ->
				{mnesia:write(Old_record#ause_login_kind{
					description = case proplists:get_value(description,New_values) of
						undefined -> Old_record#ause_login_kind.description;
						New_description -> New_description
					end,
					rule = case proplists:get_value(rule,New_values) of
						undefined -> Old_record#ause_login_kind.rule;
						New_rule -> New_rule
					end
				}),Id}
		end
	end) of
		{atomic,Success} -> Success;
		Result -> Result
	end.


%% ----------------------------
%% @doc Read ause_login_kind by id
-spec read(Id) -> {norow,Id} | {ok,ause_login_kind()}
	when
	Id :: ause_login_kind_id().

read(Id) -> a_mnesia:dirty_read(?TABLE,Id).


%% ----------------------------
%% @doc Create ause_login_kind
-spec create(Record) ->
	{ok,ause_login_kind_id()} | {existed,ause_login_kind_id()} | {aborted,_Reason}
	when
	Record :: ause_login_kind().

create(Record) when is_record(Record,ause_login_kind) ->
	case a_mnesia:transaction_create_unique(Record) of
		{atomic,ok} -> {ok,Record#ause_login_kind.id};
		{atomic,existed} -> {existed,Record#ause_login_kind.id};
		Result -> Result
	end.