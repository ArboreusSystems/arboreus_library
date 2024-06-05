%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 05. Jun 2024 10:44
%%%-------------------------------------------------------------------
-module(a_base64).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% API
-export([
	test/0,
	term_from_string/1,
	string_from_term/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Return erlang term from encoded base64 string

term_from_string(STRING) ->

	a_string:to_term(
		base64:decode(STRING)
	).


%% ----------------------------
%% @doc Return base64 string within encoded term

string_from_term(TERM) ->

	base64:encode_to_string(
		a_string:from_term(TERM)
	).
