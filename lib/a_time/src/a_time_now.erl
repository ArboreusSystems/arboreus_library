%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2019, http://arboreus.system
%%% @doc Arboreus current time handler
%%%
%%% @end
%%% Created : 01/11/2019 at 19:14
%%%-------------------------------------------------------------------
-module(a_time_now).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types
-include("../../data_models/types/types_general.hrl").
-include("../../data_models/types/types_time.hrl").

%% Data models

%% API
-export([
	test/0,
	load_nif/1,
	microseconds/0,milliseconds/0,seconds/0,
	integer/0,integer_date/0,integer_full/0,integer_extend/0,
	rfc_822/0,rfc_850/0,ansi/0
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_time_now) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_time_now) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%%-----------------------------------
%% @doc Load NIF part for this module
-spec load_nif(Path) -> ok
	when
	Path :: unix_path().

load_nif(Path) -> erlang:load_nif(Path,0).


%%-----------------------------------
%% @doc Return current time in ANSI string. Wrapper for NIF function
-spec ansi() -> a_time_ansi() | false.

ansi() -> no_nif.


%%-----------------------------------
%% @doc Return current time in RFC850 string. Wrapper for NIF function
-spec rfc_850() -> a_time_rfc850() | false.

rfc_850() -> no_nif.


%%-----------------------------------
%% @doc Return current time in RFC822 string. Wrapper for NIF function
-spec rfc_822() -> a_time_rfc822() | false.

rfc_822() -> no_nif.


%%-----------------------------------
%% @doc Return current full time in integer. Wrapper for NIF function
-spec integer_extend() -> a_time_integer_extend() | false.

integer_extend() -> no_nif.


%%-----------------------------------
%% @doc Return current full time in integer. Wrapper for NIF function
-spec integer_full() -> a_time_integer_full() | false.

integer_full() -> no_nif.


%%-----------------------------------
%% @doc Return current date in integer. Wrapper for NIF function
-spec integer_date() -> a_time_integer_date() | false.

integer_date() -> no_nif.


%%-----------------------------------
%% @doc Return current time in integer. Wrapper for NIF function
-spec integer() -> a_time_integer() | false.

integer() -> no_nif.


%%-----------------------------------
%% @doc Return current UNIX timestamp in seconds. Wrapper for NIF function
-spec seconds() -> a_time_unix_seconds() | false.

seconds() -> no_nif.


%%-----------------------------------
%% @doc Return current UNIX timestamp in milliseconds. Wrapper for NIF function
-spec milliseconds() -> a_time_unix_milliseconds() | false.

milliseconds() -> no_nif.


%%-----------------------------------
%% @doc Return current UNIX timestamp in microseconds. Wrapper for NIF function
-spec microseconds() -> a_time_unix_microseconds() | false.

microseconds() -> no_nif.