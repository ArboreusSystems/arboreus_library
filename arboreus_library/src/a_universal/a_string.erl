%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2023, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 02. Dec 2023 18:59
%%%-------------------------------------------------------------------
-module(a_string).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

-include("a_includes.hrl").

%% API
-export([
	test/0,
	from_term/1,
	to_term/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->

	STRING = "{a_test,1,\"test_string\"}.",
	TERM = {a_test,1,"test_string"},

	STRING = from_term(TERM),
	{ok,TERM} = to_term(STRING),

	ok.


%% ----------------------------
%% @doc Convert any erlang term to string
-spec from_term(TERM) -> STRING
	when
		STRING :: a_utf_text_string(),
		TERM :: term().

from_term(TERM) ->

	lists:flatten(io_lib:format("~p.",[TERM])).


%% ----------------------------
%% @doc Return term from string
-spec to_term(STRING) -> {ok,TERM} | {error,REASON}
	when
		STRING :: a_utf_text_string() | a_utf_text_binary(),
		TERM :: term(),
		REASON :: term().

to_term(BINARY) when is_binary(BINARY) ->

	to_term(binary_to_list(BINARY));

to_term(STRING) when is_list(STRING) ->

	{ok,TOKENS,_} = erl_scan:string(STRING),
	erl_parse:parse_term(TOKENS).