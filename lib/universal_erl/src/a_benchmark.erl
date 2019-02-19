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
	do/4
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
%% @doc Run the benchmark calculation for specified function
-spec do(Module,Function,Parameters,Iterations) -> {Time_start,Time_stop,Time_test}
	when
	Module :: module(),
	Function :: atom(),
	Parameters :: list_of_parameters(),
	Iterations :: integer(),
	Time_start :: pos_integer(),
	Time_stop :: pos_integer(),
	Time_test :: pos_integer().
	
do(Module,Function,Parameters,Iterations) ->
	Time_start = a_time:current(timestamp),
	do_handler(Module,Function,Parameters,Iterations),
	Time_stop = a_time:current(timestamp),
	{Time_start,Time_stop,Time_stop - Time_start}.


%% ----------------------------
%% @doc Benchmark calculation handler
-spec do_handler(Module,Function,Parameters,Iteration) -> ok
	when
	Module :: module(),
	Function :: atom(),
	Parameters :: list_of_parameters(),
	Iterations :: integer().

do_handler(_,_,_,0) -> ok;
do_handler(Module,Function,Parameters,Iteration) ->
	apply(Module,Function,Parameters),
	do_handler(Module,Function,Parameters,Iteration - 1).