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
	from_file/1,
	from_string/1,
	from_binary/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_term) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Term_string = "[1,2,3,4,5].",
	Term = [1,2,3,4,5],
	Term = from_string(Term_string),
	io:format("DONE! Parsing from string functionality test passed.~n"),
	Term_binary = <<("[1,2,3,4,5].")/utf8>>,
	Term = from_binary(Term_binary),
	io:format("DONE! Parsing from utf binary functionality test passed.~n"),
	{ok,Path} = file:get_cwd(),
	Full_path = lists:concat([Path,"/a_term.test"]),
	{ok,Full_path} = to_file(Full_path,Term),
	{ok,Term} = from_file(Full_path),
	ok = file:delete(Full_path),
	io:format("DONE! Term file storing/reading functionality test passed.~n"),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_term) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Parse term from utf binary string
-spec from_binary(Binary) -> term()
	when
	Binary :: utf_text_binary().

from_binary(Binary) ->
	from_string(unicode:characters_to_list(Binary)).


%% ----------------------------
%% @doc Parse term from string
-spec from_string(String) -> term()
	when
	String :: string().

from_string(String) ->
	{ok,Raw_term,_} = erl_scan:string(String),
	{ok,Term} = erl_parse:parse_term(Raw_term),
	Term.


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
