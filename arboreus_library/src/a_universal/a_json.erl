%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 24. Dec 2024 19:19
%%%-------------------------------------------------------------------
-module(a_json).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% System includes

%% Application includes
-include("a_includes.hrl").

%% API
-export([

	test/0,

	encode/1,
	decode/1,decode/2,

	from_file/1,from_file/2,
	to_file/2

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Encode JSON from Erlang value
-spec encode(DATA) -> OUTPUT
	when
		DATA :: map() | proplists:proplist(),
		OUTPUT :: iodata().

encode(DATA) when is_list(DATA) ->

	encode(proplists:to_map(DATA));

encode(DATA) when is_map(DATA) ->

	json:encode(DATA).


%% ----------------------------
%% @doc Perform JSON data decode from UTF string or binary
-spec decode(DATA) -> OUTPUT
	when
		DATA :: a_utf_text_binary() | a_utf_text_string(),
		OUTPUT :: map() | proplists:proplist().

decode(DATA) -> decode(DATA,map).


%% ----------------------------
%% @doc Perform JSON data decode from UTF string or binary
-spec decode(DATA,RETURN_TYPE) -> OUTPUT
	when
		DATA :: a_utf_text_binary() | a_utf_text_string(),
		RETURN_TYPE :: map | proplist,
		OUTPUT :: proplists:proplist() | map().

decode(DATA,RETURN_TYPE) when is_list(DATA) ->

	decode(list_to_binary(DATA),RETURN_TYPE);

decode(DATA,RETURN_TYPE) when is_binary(DATA) ->

	JSON = json:decode(DATA),
	case RETURN_TYPE of
		proplist -> maps:to_list(JSON);
		_ -> JSON
	end.


%% ----------------------------
%% @doc Read JSON file from file
-spec from_file(PATH) -> {ok,JSON} | {not_existed,PATH}
	when
		PATH :: a_utf_text_string(),
		JSON :: map() | proplists:proplist().

from_file(PATH) -> from_file(PATH,map).


%% ----------------------------
%% @doc Read JSON file from file
-spec from_file(PATH,RETURN_TYPE) -> {ok,JSON} | {not_existed,PATH}
	when
		PATH :: a_utf_text_string(),
		RETURN_TYPE :: map | proplist,
		JSON :: map() | proplists:proplist().

from_file(PATH,RETURN_TYPE) ->

	case filelib:is_file(PATH) of
		true ->
			{ok,DATA} = file:read_file(PATH),
			{ok,decode(DATA,RETURN_TYPE)};
		false ->
			{not_existed,PATH}
	end.


%% ----------------------------
%% @doc Write data to JSON file
-spec to_file(PATH,DATA) -> ok
	when
		PATH :: a_unix_path_string(),
		DATA :: map() | proplists:proplist().

to_file(PATH,DATA) ->

	{ok,FILE} = file:open(PATH,[write]),
	file:write(FILE,encode(DATA)),
	file:close(FILE).