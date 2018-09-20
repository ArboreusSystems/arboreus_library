%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Erlang terms handler
%%%
%%% @end
%%% Created : 24. Май 2018 12:31
%%%-------------------------------------------------------------------
-module(a_term).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Data types
-include("../data_models/types/types_general.hrl").

%% API
-export([
	test/0,
	to_file/2,
	from_file/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Write any term to file like binary
-spec to_file(Path,Term) -> {ok,Path} | {error,_Reason}
	when
	Path :: unix_path_string(),
	Term :: term().

to_file(Path,Term) ->
	case file:write_file(Path,term_to_binary(Term)) of
		ok -> {ok,Path};
		Result -> Result
	end.


%% ----------------------------
%% @doc Read term from file
-spec from_file(Path) -> {ok,term()} | {error,_Reason}
	when
	Path :: unix_path_string().

from_file(Path) ->
	case file:read_file(Path) of
		{ok,Binary} -> {ok,binary_to_term(Binary)};
		Result -> Result
	end.
