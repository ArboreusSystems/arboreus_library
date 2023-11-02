%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Erlang execution statistic module
%%%
%%% @end
%%% Created : 02. Янв. 2018 17:24
%%%-------------------------------------------------------------------
-module(a_stat).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% API
-export([
	test/0,
	execution_time/1,
	execution_time/2
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Return one time execution time value
-spec execution_time(Function::function()) -> list().

execution_time(Function) when is_function(Function) ->
	Start_time = os:system_time(),
	Output = Function(),
	Stop_time = os:system_time(),
	[
		{start_time,Start_time},
		{stop_time,Stop_time},
		{execution_time,Stop_time - Start_time},
		{output,Output}
	].


%% ----------------------------
%% @doc Return the value of run function by Count times
-spec execution_time(Function,Count) -> list()
	when
	Function :: function(),
	Count :: integer().

execution_time(Function,Count) when is_function(Function) ->
	Start_time = os:system_time(),
	execution_time_handler(Function,Count),
	Stop_time = os:system_time(),
	Execution_time = Stop_time - Start_time,
	[
		{start_time,Start_time},
		{stop_time,Stop_time},
		{execution_time,Execution_time},
		{average_time,Execution_time/Count}
	].


%% ----------------------------
%% @doc Function handler for execution_time/2
-spec execution_time_handler(Function,Count) -> list()
	when
	Function :: function(),
	Count :: integer().

execution_time_handler(_,0) -> ok;
execution_time_handler(Function,Count) ->
	Function(),
	execution_time_handler(Function,Count-1).