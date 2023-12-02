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

%% API
-export([
	test/0,
	from_term/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Convert any erlang term to string
-spec from_term(TERM) -> string()
	when TERM :: term().

from_term(TERM) ->lists:flatten(io_lib:format("~p",[TERM])).