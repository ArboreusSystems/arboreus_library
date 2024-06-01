%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus numbers handler
%%%
%%% @end
%%% Created : 07/11/2018 at 12:02
%%%-------------------------------------------------------------------
-module(a_numbers).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types
-include("a_includes.hrl").

%% Data models

%% API
-export([
	test/0,
	percentage/1,percentage/2,
	range/1,range/2
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_numbers) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Number1 = 100, Number2 = 50,
	Percentage1 = 50.0, Percentage2 = 100.0,
	Percentage1 = percentage(Number1,Number2),
	Percentage1 = percentage(Number2,Number1),
	Percentage2 = percentage(Number1,Number1),
	Percentage2 = percentage(Number2,Number2),
	io:format("DONE! Testing percentage/1 finished~n"),
	Numbers1 = {0,10},Numbers2 = {-1,9},
	Numbers3 = {0,0},Numbers4 = {-1,-1},
	Range1 = 10,Range2 = 0,
	Range1 = range(Numbers1),Range1 = range(Numbers2),
	Range2 = range(Numbers3),Range2 = range(Numbers4),
	io:format("DONE! Testing range/1 finished~n"),
	Range3 = 50,
	Range3 = range(Number1,Number2),
	Range3 = range(Number2,Number1),
	io:format("DONE! Testing range/2 finished~n"),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_numbers) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Calculate the range between 2 numbers
-spec range({Number1,Number2}) -> Range
	when
	Range :: pos_integer(),
	Number1 :: number(),
	Number2 :: number().

range({Number1,Number2}) when Number1 < Number2 ->
	Number2 - Number1;
range({Number1,Number2}) when Number1 > Number2 ->
	Number1 - Number2;
range({Number1,Number2}) when Number1 == Number2 ->
	0.


%% ----------------------------
%% @doc Calculate the range between 2 numbers
-spec range(Number1,Number2) -> Range
	when
	Range :: pos_integer(),
	Number1 :: number(),
	Number2 :: number().

range(Number1,Number2) when Number1 < Number2 ->
	Number2 - Number1;
range(Number1,Number2) when Number1 > Number2 ->
	Number1 - Number2;
range(Number1,Number2) when Number1 == Number2 ->
	0.


%% ----------------------------
%% @doc Calculate percentage less in bigger
-spec percentage({Number1,Number2}) -> number()
	when
	Number1 :: number(),
	Number2 :: number().

percentage({Number1,Number2}) when Number1 > Number2 ->
	Number2 / Number1 * 100;
percentage({Number1,Number2}) when Number1 < Number2 ->
	Number1 / Number2 * 100;
percentage({Number1,Number2}) when Number1 == Number2 ->
	100.0.


%% ----------------------------
%% @doc Calculate percentage less in bigger
-spec percentage(Number1,Number2) -> number()
	when
	Number1 :: number(),
	Number2 :: number().

percentage(Number1,Number2) when Number1 > Number2 ->
	Number2 / Number1 * 100;
percentage(Number1,Number2) when Number1 < Number2 ->
	Number1 / Number2 * 100;
percentage(Number1,Number2) when Number1 == Number2 ->
	100.0.