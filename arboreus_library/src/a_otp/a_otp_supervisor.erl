%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2023, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 28. Nov 2023 23:58
%%%-------------------------------------------------------------------
-module(a_otp_supervisor).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% Includes
-include("a_includes.hrl").

%% API
-export([
	test/0,
	find_child/3,find_supervisor_child/2,
	first_child/1,
	find_and_remove_child/3,
	remove_child/2
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Return child process reference
-spec find_child(BY_TYPE,SUPERVISOR_PID,BY_DATA) -> CHILD_REFERENCE | false
	when
		CHILD_REFERENCE :: {ID,CHILD_PID,TYPE,MODULES},
		BY_TYPE :: by_id | by_pid,
		SUPERVISOR_PID :: pid(),
		BY_DATA :: ID | CHILD_PID,
		ID :: atom(),
		CHILD_PID :: pid(),
		TYPE :: worker | supervisor,
		MODULES :: [module()].

find_child(by_id,SUPERVISOR_PID,ID) ->

	lists:keyfind(ID,1,supervisor:which_children(SUPERVISOR_PID));

find_child(by_pid,SUPERVISOR_PID,CHILD_PID) ->

	lists:keyfind(CHILD_PID,2,supervisor:which_children(SUPERVISOR_PID));

find_child(_,_,_) ->

	false.


%% ----------------------------
%% @doc Return child process from application supervisor
-spec find_supervisor_child(MODULE,PROCESS_ID)-> {ok,PROCESS_PID} | {error,REASON}
	when
		MODULE :: module(),
		PROCESS_ID :: atom() | a_utf_text_string() | a_utf_text_binary(),
		PROCESS_PID :: pid(),
		REASON :: term().

find_supervisor_child(MODULE,PROCESS_ID) ->

	case application:get_supervisor(MODULE) of
		{ok,APPLICATION_SUPERVISOR_PID} ->
			case find_child(by_id,APPLICATION_SUPERVISOR_PID,PROCESS_ID) of
				{PROCESS_ID,CHILD_PID,_TYPE,_MODULES} -> {ok,CHILD_PID};
				false -> {error,no_child}
			end;
		{error,GET_SUPERVISOR_ERROR} ->
			{error,{no_supervisor,GET_SUPERVISOR_ERROR}}
	end.


%% ----------------------------
%% @doc Return first child process reference
-spec first_child(SUPERVISOR_PID) -> FIRST_CHILD_REFERENCE | false
	when
		SUPERVISOR_PID :: pid(),
		FIRST_CHILD_REFERENCE :: {ID,CHILD_PID,TYPE,MODULES},
		ID :: atom(),
		CHILD_PID :: pid(),
		TYPE :: worker | supervisor,
		MODULES :: [module()].

first_child(SUPERVISOR_PID) when is_pid(SUPERVISOR_PID) ->

	[CHILD_PID | _] = supervisor:which_children(SUPERVISOR_PID),
	CHILD_PID;

first_child(_) ->

	{error,wrong_parameters}.


%% ----------------------------
%% @doc Find and remove child from defined supervisor
-spec find_and_remove_child(TYPE,SUPERVISOR_PID,CHILD_ID) -> ok | {error,REASON}
	when
		TYPE :: by_id | by_pid,
		SUPERVISOR_PID :: pid(),
		CHILD_ID :: pid() | term(),
		REASON :: term().

find_and_remove_child(by_id,SUPERVISOR_PID,CHILD_ID) ->

	case a_otp_supervisor:find_child(by_id,SUPERVISOR_PID,CHILD_ID) of
		{CHILD_ID,_,_,_} ->remove_child(SUPERVISOR_PID,CHILD_ID);
		_ -> {error,no_child}
	end;

find_and_remove_child(by_pid,SUPERVISOR_PID,CHILD_PID) ->

	case a_otp_supervisor:find_child(by_pid,SUPERVISOR_PID,CHILD_PID) of
		{CHILD_ID,CHILD_PID,_,_} -> remove_child(SUPERVISOR_PID,CHILD_ID);
		_ -> {error,no_child}
	end;

find_and_remove_child(_,_,_) ->

	{error,wrong_parameters}.


%% ----------------------------
%% @doc Remove child from defined supervisor
-spec remove_child(SUPERVISOR_PID,CHILD_ID) -> ok | {error,REASON}
	when
		SUPERVISOR_PID :: pid(),
		CHILD_ID :: term(),
		REASON :: term().

remove_child(SUPERVISOR_PID,CHILD_ID) ->

	case supervisor:terminate_child(SUPERVISOR_PID,CHILD_ID) of
		ok ->
			case supervisor:delete_child(SUPERVISOR_PID,CHILD_ID) of
				ok ->
					case a_otp_supervisor:find_child(by_id,SUPERVISOR_PID,CHILD_ID) of
						false -> ok;
						REPLY -> {error,{child_still_exists,REPLY}}
					end;
				{error,REASON} ->
					{error,{child_not_deleted,REASON}}
			end;
		{error,REASON} ->
			{error,{child_not_terminated,REASON}}
	end.