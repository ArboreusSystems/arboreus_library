%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus recorded numbers handler
%%%
%%% @end
%%% Created : 07/11/2018 at 16:20
%%%-------------------------------------------------------------------
-module(a_numbers_r).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types
-include("../../data_models/types/types_general.hrl").

%% Data models
-record(test,{a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10}).

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
		"Module (a_numbers_r) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Numbers1 = #test{
		a0 = 0,a1 = 1,a2 = 2,a3 = 3,a4 = 4,
		a5 = 5,a6 = 6,a7 = 7,a8 = 8,a9 = 9,a10 = 10
	},
	Numbers2 = #test{
		a0 = -1,a1 = 0,a2 = 1,a3 = 2,a4 = 3,
		a5 = 4,a6 = 5,a7 = 6,a8 = 7,a9 = 8,a10 = 9},
	Endpoints1 = {0,10},
	Endpoints2 = {-1,9},
	Endpoints1 = endpoints(Numbers1),
	Endpoints2 = endpoints(Numbers2),
	io:format("DONE! Testing endpoints/1 finished~n"),
	Range = 10,
	Range = range(Numbers1),
	Range = range(Numbers2),
	io:format("DONE! Testing range/1 finished~n"),
	Percentage1 = {test,0.0,10.0,20.0,30.0,40.0,50.0,60.0,70.0,80.0,90.0,100.0},
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
		"Module (a_numbers_r) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Calculate average value for the record of numbers
-spec average(Record) -> number()
	when
	Record :: record().

average(Record) -> average_handler(Record,tuple_size(Record),0).


%% ----------------------------
%% @doc The average procedure handler
-spec average_handler(Record,Counter,Sum) -> Sum
	when
	Record :: record(),
	Counter :: pos_integer(),
	Sum :: 0.

average_handler(Tuple,1,Sum) -> Sum/(tuple_size(Tuple) - 1);
average_handler(Tuple,Counter,Sum) ->
	average_handler(
		Tuple,Counter - 1,
		Sum + element(Counter,Tuple)
	).


%% ----------------------------
%% @doc Generate avarage percentage value of the record of numbers
-spec average_percentage(Record) -> number()
	when
	Record :: tuple().

average_percentage(Record) -> average(percentage(Record)).


%% ----------------------------
%% @doc Define the bigger value and calculate percentage to each of numbers of the record
-spec percentage(Record) -> record()
	when
	Record :: record().

percentage(Record) ->
	{Minimal,Maximal} = endpoints(Record),
	Range = a_numbers:range({Minimal,Maximal}),
	Delta = case Minimal < 0 of
		true -> 0 - Minimal;
		_ -> 0
	end,
	percentage_handler(Range,Delta,tuple_size(Record),Record).


%% ----------------------------
%% @doc The percentage procedure handler
-spec percentage_handler(Range,Delta,Counter,Record) -> record()
	when
	Range :: pos_integer(),
	Delta :: number(),
	Counter :: pos_integer(),
	Record :: record().

percentage_handler(_,_,1,Record) -> Record;
percentage_handler(Range,Delta,Counter,Record) ->
	percentage_handler(
		Range,Delta,Counter - 1,
		setelement(
			Counter,Record,
			a_numbers:percentage(element(Counter,Record) + Delta,Range)
		)).


%% ----------------------------
%% @doc Calculate the range between endpoints of record of numbers
-spec range(Record) -> Range
	when
	Record :: record(),
	Range :: pos_integer().

range(Record) -> a_numbers:range(endpoints(Record)).


%% ----------------------------
%% @doc Find endpoints values of the list of numbers, less and bigger
-spec endpoints(Record) -> {Minimal,Maximal}
	when
	Record :: record(),
	Minimal :: number(),
	Maximal :: number().

endpoints({_,Number1,Number2}) when Number1 >= Number2 ->
	{Number2,Number1};
endpoints({_,Number1,Number2}) when Number1 < Number2 ->
	{Number1,Number2};
endpoints(Record) ->
	Length = tuple_size(Record),
	First = element(2,Record),
	Second = element(3,Record),
	case First >= Second of
		true -> Maximal = First, Minimal = Second;
		_ -> Maximal = Second, Minimal = First
	end,
	endpoints_handler(4,Length,Minimal,Maximal,Record).


%% ----------------------------
%% @doc The endpoints functionality handler
-spec endpoints_handler(Counter,Length,Minimal,Maximal,Record) -> {Minimal,Maximal}
	when
	Counter :: pos_integer(),
	Length :: pos_integer(),
	Minimal :: number(),
	Maximal :: number(),
	Record :: record().

endpoints_handler(Counter,Length,Minimal,Maximal,_) when Counter == Length + 1 ->
	{Minimal,Maximal};
endpoints_handler(Counter,Length,Minimal,Maximal,Record) ->
	Number = element(Counter,Record),
	case Number >= Maximal of
		true -> Maximal_out = Number,Minimal_out = Minimal;
		_ ->
			case Number =< Minimal of
				true -> Maximal_out = Maximal,Minimal_out = Number;
				_ -> Maximal_out = Maximal,Minimal_out = Minimal
			end
	end,
	endpoints_handler(Counter + 1,Length,Minimal_out,Maximal_out,Record).