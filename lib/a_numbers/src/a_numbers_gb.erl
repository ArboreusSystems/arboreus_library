%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus gb-treed numbers handler
%%%
%%% @end
%%% Created : 07/11/2018 at 19:48
%%%-------------------------------------------------------------------
-module(a_numbers_gb).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types
-include("../../data_models/types/types_general.hrl").

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
		"Module (a_numbers_gb) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Numbers1 = gb_trees:from_orddict([
		{a1,0},{a2,1},{a3,2},{a4,3},{a5,4},{a6,5},
		{a7,6},{a8,7},{a9,8},{a10,9},{a11,10}
	]),
	Numbers2 = gb_trees:from_orddict([
		{a1,-1},{a2,0},{a3,1},{a4,2},{a5,3},{a6,4},
		{a7,5},{a8,6},{a9,7},{a10,8},{a11,9}
	]),
	Endpoints1 = {0,10},
	Endpoints2 = {-1,9},
	Endpoints1 = endpoints(Numbers1),
	Endpoints2 = endpoints(Numbers2),
	io:format("DONE! Testing endpoints/1 finished~n"),
	Range = 10,
	Range = range(Numbers1),
	Range = range(Numbers2),
	io:format("DONE! Testing range/1 finished~n"),
	Percentage1 = {11,
		{a6,50.0,
			{a3,20.0,
				{a2,10.0,{a1,0.0,nil,nil},nil},
				{a5,40.0,{a4,30.0,nil,nil},nil}},
			{a9,80.0,
				{a8,70.0,{a7,60.0,nil,nil},nil},
				{a11,100.0,{a10,90.0,nil,nil},nil}}}},
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
		"Module (a_numbers_gb) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Calculate average value for the gb_tree of numbers
-spec average(Gb_tree) -> number()
	when
	Gb_tree :: gb_trees:tree().

average(Gb_tree) ->
	a_numbers_pl:average(gb_trees:to_list(Gb_tree)).


%% ----------------------------
%% @doc Generate avarage percentage value of the list of numbers
-spec average_percentage(Gb_tree) -> number()
	when
	Gb_tree :: gb_trees:tree().

average_percentage(Gb_tree) -> average(percentage(Gb_tree)).


%% ----------------------------
%% @doc Define the bigger value and calculate percentage to each of numbers of the gb_tree
-spec percentage(Gb_tree) -> gb_trees:tree()
	when
	Gb_tree :: gb_trees:tree().

percentage(Gb_tree) ->
	gb_trees:from_orddict(
		a_numbers_pl:percentage(gb_trees:to_list(Gb_tree))
	).

%% ----------------------------
%% @doc Calculate the range between endpoints of gb_tree of numbers
-spec range(Gb_tree) -> Range
	when
	Gb_tree :: gb_trees:tree(),
	Range :: pos_integer().

range(Gb_tree) -> a_numbers:range(endpoints(Gb_tree)).


%% ----------------------------
%% @doc Find endpoints values of the gb_tree of numbers, less and bigger
-spec endpoints(Gb_tree) -> {Minimal,Maximal}
	when
	Gb_tree :: gb_trees:tree(),
	Minimal :: number(),
	Maximal :: number().

endpoints(Gb_tree) ->
	a_numbers_pl:endpoints(gb_trees:to_list(Gb_tree)).