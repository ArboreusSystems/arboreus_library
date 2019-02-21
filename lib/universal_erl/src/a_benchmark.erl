%%% -------------------------------------------------------------------
%%% @doc The benchmark module.
%%% @notice
%%%
%%% @copyright Arboreus (http://arboreus.systems)
%%% @author Alexandr Kirilov (http://alexandr.kirilov.me)
%%% @created 02/19/2019 at 20:42
%%% -------------------------------------------------------------------
-module(a_benchmark).
-author("Alexandr Kirilov (http://alexandr.kirilov.me)").

%% Constants

%% Data types
-include("../../data_models/types/types_general.hrl").

%% Data models

%% API
-export([
	test/0,
	do/4,
	set/2
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_benchmark) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_benchmark) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.



%% ----------------------------
%% @doc Run the set of functions for benchmark
-spec set(Functions,Iterations) -> list_of_tuples()
	when
	Functions :: [{Module,Function,Parameters}],
	Iterations :: pos_integer(),
	Module :: atom(),
	Function :: atom(),
	Parameters :: list_of_parameters().

set(Functions,Iterations) -> set_handler(Functions,Iterations,[]).


%% ----------------------------
%% @doc Run set functionality handler
-spec set_handler([{Module,Function,Parameters}],Iterations,Output) -> {ok,Output}
	when
	Module :: atom(),
	Function :: atom(),
	Parameters :: list_of_parameters(),
	Iterations :: pos_integer(),
	Output :: list_of_tuples().
	

set_handler([],_,Output) -> {ok,lists:keysort(1,Output)};
set_handler([{Module,Function,Parameters}|Functions],Iterations,Output) ->
	set_handler(Functions,Iterations,lists:append([
		Output,[
			try do(Module,Function,Parameters,Iterations)
			catch _:_ -> {failed,0,Module,Function,Parameters,0,0} end
		]
	])).


%% ----------------------------
%% @doc Run the benchmark calculation for specified function
-spec do(Module,Function,Parameters,Iterations) ->
	{Time_iteration,Time_test,Module,Function,Parameters,Time_start,Time_stop}
	when
	Module :: module(),
	Function :: atom(),
	Parameters :: list_of_parameters(),
	Iterations :: integer(),
	Time_iteration :: float_pos(),
	Time_test :: pos_integer(),
	Time_start :: pos_integer(),
	Time_stop :: pos_integer().
	
do(Module,Function,Parameters,Iterations) ->
	Time_start = a_time_now:microseconds(),
	do_handler(Module,Function,Parameters,Iterations),
	Time_stop = a_time_now:microseconds(),
	Test_time = Time_stop - Time_start,
	{Test_time/Iterations,Test_time,Module,Function,Parameters,Time_start,Time_stop}.


%% ----------------------------
%% @doc Benchmark calculation handler
-spec do_handler(Module,Function,Parameters,Iterations) -> ok
	when
	Module :: module(),
	Function :: atom(),
	Parameters :: list_of_parameters(),
	Iterations :: integer().

do_handler(_,_,_,0) -> ok;
do_handler(Module,Function,Parameters,Iteration) ->
	apply(Module,Function,Parameters),
	do_handler(Module,Function,Parameters,Iteration - 1).