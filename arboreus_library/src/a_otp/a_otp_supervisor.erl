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

%% API
-export([
	test/0,
	find_child/3,
	first_child/1
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
	CHILD_PID.