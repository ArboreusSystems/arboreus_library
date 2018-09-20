%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc
%%%
%%% @end
%%% Created : 06/06/2018 at 11:59
%%%-------------------------------------------------------------------
-module(regular_module).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types

%% Data models

%% API
-export([
	test/0
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (~p) testing started at:~n~p (~p)~n",
		[?MODULE,a_time:from_timestamp(rfc850,Time_start),Time_start]
	),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (~p) testing finished at:~n~p (~p)~n",
		[?MODULE,a_time:from_timestamp(rfc850,Time_stop),Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.