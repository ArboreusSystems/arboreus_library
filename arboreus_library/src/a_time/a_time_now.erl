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
-include("../include/types/types_general.hrl").
-include("../include/types/types_time.hrl").

%% Data models

%% API
-export([
	test/0,
	microseconds/0,milliseconds/0,seconds/0,
	integer/0,integer_date/0,integer_full/0,integer_extend/0,
	rfc_822/0,rfc_850/0,ansi/0
]).

%% On load
-on_load(init/0).

%% Definitions
-define(A_TIME_NOW_APPLICATION, arboreus_library).
-define(A_TIME_NOW_LIBRARY, a_time_now).


%% ----------------------------
%% @doc Load NIF library for module
-spec init() -> ok | {error, {REASON, TEXT}}
	when
		REASON :: term(),
		TEXT :: string().

init() ->

	NIF_PATH = case code:priv_dir(?A_TIME_NOW_APPLICATION) of
		{error, bad_name} ->
			case filelib:is_dir(filename:join(["..",priv])) of
				true -> filename:join(["..",priv,?A_TIME_NOW_LIBRARY]);
				_ -> filename:join([priv,?A_TIME_NOW_LIBRARY])
			end;
		DIR ->
			filename:join(DIR,?A_TIME_NOW_LIBRARY)
	end,
	erlang:load_nif(NIF_PATH,0).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->

	TIME_START = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_time_now) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850,TIME_START),TIME_START]
	),
	TIME_STOP = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_time_now) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850,TIME_STOP),TIME_STOP]
	),
	io:format("Test time is: ~p~n", [TIME_STOP - TIME_START]),
	ok.


%%-----------------------------------
%% @doc Return current time in ANSI string. Wrapper for NIF function
-spec ansi() -> a_time_ansi() | false.

ansi() -> a_error:nif_not_loaded(?MODULE,?LINE).


%%-----------------------------------
%% @doc Return current time in RFC850 string. Wrapper for NIF function
-spec rfc_850() -> a_time_rfc850() | false.

rfc_850() -> a_error:nif_not_loaded(?MODULE,?LINE).


%%-----------------------------------
%% @doc Return current time in RFC822 string. Wrapper for NIF function
-spec rfc_822() -> a_time_rfc822() | false.

rfc_822() -> a_error:nif_not_loaded(?MODULE,?LINE).


%%-----------------------------------
%% @doc Return current full time in integer. Wrapper for NIF function
-spec integer_extend() -> a_time_integer_extend() | false.

integer_extend() -> a_error:nif_not_loaded(?MODULE,?LINE).


%%-----------------------------------
%% @doc Return current full time in integer. Wrapper for NIF function
-spec integer_full() -> a_time_integer_full() | false.

integer_full() -> a_error:nif_not_loaded(?MODULE,?LINE).


%%-----------------------------------
%% @doc Return current date in integer. Wrapper for NIF function
-spec integer_date() -> a_time_integer_date() | false.

integer_date() -> a_error:nif_not_loaded(?MODULE,?LINE).


%%-----------------------------------
%% @doc Return current time in integer. Wrapper for NIF function
-spec integer() -> a_time_integer() | false.

integer() -> a_error:nif_not_loaded(?MODULE,?LINE).


%%-----------------------------------
%% @doc Return current UNIX timestamp in seconds. Wrapper for NIF function
-spec seconds() -> a_time_unix_seconds() | false.

seconds() -> a_error:nif_not_loaded(?MODULE,?LINE).


%%-----------------------------------
%% @doc Return current UNIX timestamp in milliseconds. Wrapper for NIF function
-spec milliseconds() -> a_time_unix_milliseconds() | false.

milliseconds() -> a_error:nif_not_loaded(?MODULE,?LINE).


%%-----------------------------------
%% @doc Return current UNIX timestamp in microseconds. Wrapper for NIF function
-spec microseconds() -> a_time_unix_microseconds() | false.

microseconds() -> a_error:nif_not_loaded(?MODULE,?LINE).