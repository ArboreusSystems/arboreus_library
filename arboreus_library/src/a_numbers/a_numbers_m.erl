%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus maped numbers handler
%%%
%%% @end
%%% Created : 07/11/2018 at 18:15
%%%-------------------------------------------------------------------
-module(a_numbers_m).
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
		"Module (a_numbers_m) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Numbers1 = #{
		a1 => 0,a2 => 1,a3 => 2,a4 => 3,a5 => 4,a6 => 5,
		a7 => 6,a8 => 7,a9 => 8,a10 => 9,a11 => 10
	},
	Numbers2 = #{
		a1 => -1,a2 => 0,a3 => 1,a4 => 2,a5 => 3,a6 => 4,
		a7 => 5,a8 => 6,a9 => 7,a10 => 8,a11 => 9
	},
	Endpoints1 = {0,10},
	Endpoints2 = {-1,9},
	Endpoints1 = endpoints(Numbers1),
	Endpoints2 = endpoints(Numbers2),
	io:format("DONE! Testing endpoints/1 finished~n"),
	Range = 10,
	Range = range(Numbers1),
	Range = range(Numbers2),
	io:format("DONE! Testing range/1 finished~n"),
	Percentage1 = #{
		a1 => 0.0,a10 => 90.0,a11 => 100.0,a2 => 10.0,a3 => 20.0,
		a4 => 30.0,a5 => 40.0,a6 => 50.0,a7 => 60.0,a8 => 70.0,a9 => 80.0
	},
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
		"Module (a_numbers_m) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Calculate average value for the map of numbers
-spec average(Map) -> number()
	when
	Map :: map().

average(Map) -> average_handler(Map,maps:keys(Map),0).


%% ----------------------------
%% @doc The average procedure handler
-spec average_handler(Map,Keys,Sum) -> Sum
	when
	Map :: map(),
	Keys :: a_list_of_values(),
	Sum :: 0.

average_handler(Map,[],Sum) -> Sum/maps:size(Map);
average_handler(Map,[Key|Keys],Sum) ->
	average_handler(Map,Keys,Sum + maps:get(Key,Map)).


%% ----------------------------
%% @doc Generate avarage percentage value of the list of numbers
-spec average_percentage(Map) -> number()
	when
	Map :: map().

average_percentage(Map) -> average(percentage(Map)).


%% ----------------------------
%% @doc Define the bigger value and calculate percentage to each of numbers of the map
-spec percentage(Map) -> map()
	when
	Map :: map().

percentage(Map) ->
	{Minimal,Maximal} = endpoints(Map),
	Range = a_numbers:range({Minimal,Maximal}),
	Delta = case Minimal < 0 of
		true -> 0 - Minimal;
		_ -> 0
	end,
	percentage_handler(Map,maps:keys(Map),Range,Delta).


%% ----------------------------
%% @doc The percentage procedure handler
-spec percentage_handler(Map,Keys,Range,Delta) -> map()
	when
	Map :: map(),
	Keys :: a_list_of_values(),
	Range :: number(),
	Delta :: number().

percentage_handler(Map,[],_,_) -> Map;
percentage_handler(Map,[Key|Keys],Range,Delta) ->
	percentage_handler(
		maps:update(
			Key,a_numbers:percentage(
				Range,maps:get(Key,Map) + Delta
		),Map),
		Keys,Range,Delta
	).


%% ----------------------------
%% @doc Calculate the range between endpoints of map of numbers
-spec range(Map) -> Range
	when
	Map :: map(),
	Range :: pos_integer().

range(Map) -> a_numbers:range(endpoints(Map)).


%% ----------------------------
%% @doc Find endpoints values of the map of numbers, less and bigger
-spec endpoints(Map) -> {Minimal,Maximal}
	when
	Map :: map(),
	Minimal :: number(),
	Maximal :: number().

endpoints(Map) ->
	[First_key|[Second_key|Keys]] = maps:keys(Map),
	First = maps:get(First_key,Map),
	Second = maps:get(Second_key,Map),
	case First >= Second of
		true -> endpoints_handler(Map,Keys,Second,First);
		_ -> endpoints_handler(Map,Keys,First,Second)
	end.


%% ----------------------------
%% @doc The endpoints functionality handler
-spec endpoints_handler(Map,Keys,Minimal,Maximal) -> {Minimal,Maximal}
	when
	Map :: map(),
	Keys :: a_list_of_values(),
	Minimal :: number(),
	Maximal :: number().

endpoints_handler(_,[],Minimal,Maximal) -> {Minimal,Maximal};
endpoints_handler(Map,[Key|Keys],Minimal,Maximal) ->
	Number = maps:get(Key,Map),
	case Number >= Maximal of
		true -> endpoints_handler(Map,Keys,Minimal,Number);
		_ ->
			case Number < Minimal of
				true -> endpoints_handler(Map,Keys,Number,Maximal);
				_ -> endpoints_handler(Map,Keys,Minimal,Maximal)
			end
	end.