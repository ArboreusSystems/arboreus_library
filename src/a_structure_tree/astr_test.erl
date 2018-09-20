%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Test module for astructures testing
%%%
%%% @end
%%% Created : 03. Май 2018 21:22
%%%-------------------------------------------------------------------
-module(astr_test).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% API
-export([
	test/0,
	run/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc

run(init) ->
	astr_mdb:init([node()]),
	run(body);
run(body) ->
	io:format(
		"~nA_structure_tree modules started at:~n ~p (~p)~n",
		[a_time:current(rfc850), a_time:current(timestamp)]
	),
	io:format("~n*** Test for astr_link data model ~n"),
	ok = astr_link:test(),
	io:format("~n*** Test for astr_alias data model ~n"),
	ok = astr_alias:test(),
	io:format("~n*** Test for astr_point data model ~n"),
	ok = astr_point:test(),
	io:format(
		"~nA_structure_tree modules finished at:~n ~p (~p)~n",
		[a_time:current(rfc850), a_time:current(timestamp)]
	),
	ok.
	
