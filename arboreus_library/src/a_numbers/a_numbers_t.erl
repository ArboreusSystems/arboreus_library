%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus tupled numbers handler
%%%
%%% @end
%%% Created : 07/11/2018 at 14:21
%%%-------------------------------------------------------------------
-module(a_numbers_t).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types
-include("a_includes.hrl").

%% Data models

%% API
-export([
	test/0,
	endpoints/1,
	range/1,
	percentage/1,average_percentage/1,
	average/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_numbers_t) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Numbers1 = {0,1,2,3,4,5,6,7,8,9,10},
	Numbers2 = {-1,0,1,2,3,4,5,6,7,8,9},
	Endpoints1 = {0,10},
	Endpoints2 = {-1,9},
	Endpoints1 = endpoints(Numbers1),
	Endpoints2 = endpoints(Numbers2),
	io:format("DONE! Testing endpoints/1 finished~n"),
	Range = 10,
	Range = range(Numbers1),
	Range = range(Numbers2),
	io:format("DONE! Testing range/1 finished~n"),
	Percentage1 = {0.0,10.0,20.0,30.0,40.0,50.0,60.0,70.0,80.0,90.0,100.0},
	Percentage1 = percentage(Numbers1),
	Percentage1 = percentage(Numbers2),
	io:format("DONE! Testing percentage/1 finished~n"),
	Average1 = 5.0,
	Average2 = 4.0,
	Average1 = average(Numbers1),
	Average2 = average(Numbers2),
	io:format("DONE! Testing average/1 finished~n"),
	Average_percentage = 50.0,
	Average_percentage = average_percentage(Numbers1),
	Average_percentage = average_percentage(Numbers2),
	io:format("DONE! Testing average_percentage/1 finished~n"),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_numbers_t) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Calculate average value for the tuple of numbers
-spec average(Tuple) -> number()
	when
	Tuple :: tuple().

average(Tuple) -> average_handler(Tuple,tuple_size(Tuple),0).


%% ----------------------------
%% @doc The average procedure handler
-spec average_handler(Tuple,Counter,Sum) -> Sum
	when
	Tuple :: tuple(),
	Counter :: pos_integer(),
	Sum :: 0.

average_handler(Tuple,0,Sum) -> Sum/tuple_size(Tuple);
average_handler(Tuple,Counter,Sum) ->
	average_handler(
		Tuple,Counter - 1,
		Sum + element(Counter,Tuple)
	).


%% ----------------------------
%% @doc Generate avarage percentage value of the list of numbers
-spec average_percentage(Tuple) -> number()
	when
	Tuple :: tuple().

average_percentage(Tuple) -> average(percentage(Tuple)).


%% ----------------------------
%% @doc Define the bigger value and calculate percentage to each of numbers of the list
-spec percentage(Tuple) -> tuple()
	when
	Tuple :: tuple().

percentage(Tuple) ->
	{Minimal,Maximal} = endpoints(Tuple),
	Range = a_numbers:range({Minimal,Maximal}),
	Delta = case Minimal < 0 of
		true -> 0 - Minimal;
		_ -> 0
	end,
	percentage_handler(Range,Delta,tuple_size(Tuple),Tuple).


%% ----------------------------
%% @doc The percentage procedure handler
-spec percentage_handler(Range,Delta,Counter,Tuple) -> tuple()
	when
	Range :: pos_integer(),
	Delta :: number(),
	Counter :: pos_integer(),
	Tuple :: tuple().

percentage_handler(_,_,0,Tuple) -> Tuple;
percentage_handler(Range,Delta,Counter,Tuple) ->
	percentage_handler(
		Range,Delta,Counter - 1,
		setelement(
			Counter,Tuple,
			a_numbers:percentage(element(Counter,Tuple) + Delta,Range)
	)).


%% ----------------------------
%% @doc Calculate the range between endpoints of list of numbers
-spec range(Tuple) -> Range
	when
	Tuple :: tuple(),
	Range :: pos_integer().

range(Tuple) -> a_numbers:range(endpoints(Tuple)).


%% ----------------------------
%% @doc Find endpoints values of the list of numbers, less and bigger
-spec endpoints(Tuple) -> {Minimal,Maximal}
	when
	Tuple :: tuple(),
	Minimal :: number(),
	Maximal :: number().

endpoints({Number1,Number2}) when Number1 >= Number2 ->
	{Number2,Number1};
endpoints({Number1,Number2}) when Number1 < Number2 ->
	{Number1,Number2};
endpoints(Tuple) ->
	Length = tuple_size(Tuple),
	First = element(1,Tuple),
	Second = element(2,Tuple),
	case First >= Second of
		true -> Maximal = First, Minimal = Second;
		_ -> Maximal = Second, Minimal = First
	end,
	endpoints_handler(3,Length,Minimal,Maximal,Tuple).


%% ----------------------------
%% @doc The endpoints functionality handler
-spec endpoints_handler(Counter,Length,Minimal,Maximal,Tuple) -> {Minimal,Maximal}
	when
	Counter :: pos_integer(),
	Length :: pos_integer(),
	Minimal :: number(),
	Maximal :: number(),
	Tuple :: tuple().

endpoints_handler(Counter,Length,Minimal,Maximal,_) when Counter == Length + 1 ->
	{Minimal,Maximal};
endpoints_handler(Counter,Length,Minimal,Maximal,Tuple) ->
	Number = element(Counter,Tuple),
	case Number >= Maximal of
		true -> Maximal_out = Number,Minimal_out = Minimal;
		_ ->
			case Number =< Minimal of
				true -> Maximal_out = Maximal,Minimal_out = Number;
				_ -> Maximal_out = Maximal,Minimal_out = Minimal
			end
	end,
	endpoints_handler(Counter + 1,Length,Minimal_out,Maximal_out,Tuple).