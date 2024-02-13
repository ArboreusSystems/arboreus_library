%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus proplisted numbers handler
%%%
%%% @end
%%% Created : 07/11/2018 at 17:41
%%%-------------------------------------------------------------------
-module(a_numbers_pl).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types
-include("../include/types/types_a_general.hrl").

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
		"Module (a_numbers_pl) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Numbers1 = [{a1,0},{a2,1},{a3,2},{a4,3},{a5,4},{a6,5},{a7,6},{a8,7},{a9,8},{a10,9},{a11,10}],
	Numbers2 = [{a1,-1},{a2,0},{a3,1},{a4,2},{a5,3},{a6,4},{a7,5},{a8,6},{a9,7},{a10,8},{a11,9}],
	Endpoints1 = {0,10},
	Endpoints2 = {-1,9},
	Endpoints1 = endpoints(Numbers1),
	Endpoints2 = endpoints(Numbers2),
	io:format("DONE! Testing endpoints/1 finished~n"),
	Range = 10,
	Range = range(Numbers1),
	Range = range(Numbers2),
	io:format("DONE! Testing range/1 finished~n"),
	Percentage1 = [
		{a1,0.0},{a2,10.0},{a3,20.0},{a4,30.0},{a5,40.0},{a6,50.0},
		{a7,60.0},{a8,70.0},{a9,80.0},{a10,90.0},{a11,100.0}
	],
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
		"Module (a_numbers_pl) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Calculate average value for the proplist of numbers
-spec average(List) -> number()
	when
	List :: a_list_of_numbers().

average(Proplist) -> average_handler(Proplist,0,0).


%% ----------------------------
%% @doc The average procedure handler
-spec average_handler(Proplist,Counter,Sum) -> Sum
	when
	Proplist :: proplists:proplist(),
	Counter :: 0,
	Sum :: 0.

average_handler([],Counter,Sum) -> Sum/Counter;
average_handler([{_,Number}|Proplist],Counter,Sum) ->
	average_handler(Proplist,Counter + 1,Sum + Number).


%% ----------------------------
%% @doc Generate avarage percentage value of the list of numbers
-spec average_percentage(Proplist) -> number()
	when
	Proplist :: proplists:proplist().

average_percentage(List) -> average(percentage(List)).


%% ----------------------------
%% @doc Define the bigger value and calculate percentage to each of numbers of the proplist
-spec percentage(Proplist) -> proplists:proplist()
	when
	Proplist :: proplists:proplist().

percentage(List) ->
	{Minimal,Maximal} = endpoints(List),
	Range = a_numbers:range({Minimal,Maximal}),
	Delta = case Minimal < 0 of
		true -> 0 - Minimal;
		_ -> 0
	end,
	[{Key,a_numbers:percentage(Range,Value + Delta)} || {Key,Value} <- List].


%% ----------------------------
%% @doc Calculate the range between endpoints of proplist of numbers
-spec range(Proplist) -> Range
	when
	Proplist :: proplists:proplist(),
	Range :: pos_integer().

range(List) -> a_numbers:range(endpoints(List)).


%% ----------------------------
%% @doc Find endpoints values of the proplist of numbers, less and bigger
-spec endpoints(Proplist) -> {Minimal,Maximal}
	when
	Proplist :: proplists:proplist(),
	Minimal :: number(),
	Maximal :: number().

endpoints(Proplist) ->
	[{_,First}|[{_,Second}|Proplist_out]] = Proplist,
	case First >= Second of
		true -> endpoints_handler(Proplist_out,Second,First);
		_ -> endpoints_handler(Proplist_out,First,Second)
	end.


%% ----------------------------
%% @doc The endpoints functionality handler
-spec endpoints_handler(Proplist,Minimal,Maximal) -> {Minimal,Maximal}
	when
	Proplist :: proplists:proplist(),
	Minimal :: number(),
	Maximal :: number().

endpoints_handler([],Minimal,Maximal) -> {Minimal,Maximal};
endpoints_handler([{_,Number}|Proplist],Minimal,Maximal) ->
	case Number >= Maximal of
		true -> endpoints_handler(Proplist,Minimal,Number);
		_ ->
			case Number < Minimal of
				true -> endpoints_handler(Proplist,Number,Maximal);
				_ -> endpoints_handler(Proplist,Minimal,Maximal)
			end
	end.