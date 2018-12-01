%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Module for running library self-test
%%%
%%% @end
%%% Created : 11/10/2018 at 18:43
%%%-------------------------------------------------------------------
-module(a_test).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants
-define(MODULES,[
	?MODULE,
	a_matrix,
	a_node,
	a_term,
	a_config,
	a_code,
	a_value_bin_is,
	a_value_str_is,
	a_structure_gb,
	a_structure_l,
	a_structure_m,
	a_structure_pl,
	a_structure_r,
	a_structure_t,
	a_numbers,
	a_numbers_gb,
	a_numbers_l,
	a_numbers_m,
	a_numbers_pl,
	a_numbers_r,
	a_numbers_t
]).

%% Data types
-include("../../data_models/types/types_general.hrl").

%% Data models

%% API
-export([
	test/0,
	run/0
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Do the whole library test
-spec run() -> ok.

run() -> run_handler(?MODULES).


%% ----------------------------
%% @doc Do the library test action handler
-spec run_handler(Modules) -> ok
	when
	Modules :: list_of_atoms().

run_handler([]) ->
	io:format("*******************************~nDONE! All tests passed.~n"),
	ok;
run_handler([Module|Modules]) ->
	ok = Module:test(),
	io:format("DONE! Module ~p test passed.~n",[Module]),
	run_handler(Modules).