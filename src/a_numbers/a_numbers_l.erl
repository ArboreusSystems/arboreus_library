%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreuse listed numbers handler module
%%%
%%% @end
%%% Created : 07/10/2018 at 23:31
%%%-------------------------------------------------------------------
-module(a_numbers_l).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types
-include("../data_models/types/types_general.hrl").

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
		"Module (a_numbers_l) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Numbers1 = [0,1,2,3,4,5,6,7,8,9,10],
	Numbers2 = [-1,0,1,2,3,4,5,6,7,8,9],
	Endpoints1 = {0,10},
	Endpoints2 = {-1,9},
	Endpoints1 = endpoints(Numbers1),
	Endpoints2 = endpoints(Numbers2),
	io:format("DONE! Testing endpoints/1 finished~n"),
	Range = 10,
	Range = range(Numbers1),
	Range = range(Numbers2),
	io:format("DONE! Testing range/1 finished~n"),
	Percentage1 = [0.0,10.0,20.0,30.0,40.0,50.0,60.0,70.0,80.0,90.0,100.0],
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
		"Module (a_numbers_l) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Calculate average value for the list of numbers
-spec average(List) -> number()
	when
	List :: list_of_numbers().

average(List) -> average_handler(List,0,0).


%% ----------------------------
%% @doc The average procedure handler
-spec average_handler(List,Counter,Sum) -> Sum
	when
	List :: list_of_numbers(),
	Counter :: 0,
	Sum :: 0.

average_handler([],Counter,Sum) -> Sum/Counter;
average_handler([Number|List],Counter,Sum) ->
	average_handler(List,Counter + 1,Sum + Number).


%% ----------------------------
%% @doc Generate avarage percentage value of the list of numbers
-spec average_percentage(List) -> number()
	when
	List :: list_of_numbers().

average_percentage(List) -> average(percentage(List)).


%% ----------------------------
%% @doc Define the bigger value and calculate percentage to each of numbers of the list
-spec percentage(List) -> list_of_numbers()
	when
	List :: list_of_numbers().

percentage(List) ->
	{Minimal,Maximal} = endpoints(List),
	Range = a_numbers:range({Minimal,Maximal}),
	Delta = case Minimal < 0 of
		true -> 0 - Minimal;
		_ -> 0
	end,
	[a_numbers:percentage(Range,Value + Delta) || Value <- List].


%% ----------------------------
%% @doc Calculate the range between endpoints of list of numbers
-spec range(List) -> Range
	when
	List :: list_of_numbers(),
	Range :: pos_integer().

range(List) -> a_numbers:range(endpoints(List)).


%% ----------------------------
%% @doc Find endpoints values of the list of numbers, less and bigger
-spec endpoints(List) -> {Minimal,Maximal}
	when
	List :: list_of_numbers(),
	Minimal :: number(),
	Maximal :: number().

endpoints(List) ->
	[Minimal|Sorted_list] = lists:sort(List),
	{Minimal,lists:last(Sorted_list)}.