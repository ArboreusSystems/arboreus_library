%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc The files handler module
%%%
%%% @end
%%% Created : 09. Февр. 2018 23:07
%%%-------------------------------------------------------------------
-module(a_file).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Data types
-include("a_includes.hrl").

%% API
-export([

	test/0,

	do_line/2,do_line/4,
	existed/1,

	create/2,create_no_check/2,
	create_empty/1,create_empty_no_check/1,
	clear/1,clear_no_check/1,
	delete/1,delete_no_check/1,

	add_to_begin/2,add_to_begin_no_check/2,
	add_to_end/2,add_to_end_no_check/2,

	add_line_to_begin/2,add_line_to_begin_no_check/2,
	add_line_to_end/2,add_line_to_end_no_check/2,
	delete_first_line/1,delete_first_line_no_check/1,
	delete_last_line/1,delete_last_line_no_check/1

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->

	TIME_START = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (eee) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850,TIME_START),TIME_START]
	),
%%	{ok,Path} = file:get_cwd(),
%%	Full_path = lists:concat([Path,"/a_file.test"]),
	
	TIME_STOP = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (eee) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850,TIME_STOP),TIME_STOP]
	),
	io:format("Test time is: ~p~n", [TIME_STOP-TIME_START]),
	ok.


%% ----------------------------
%% @doc Do the function for the line from requested file
-spec do_line(FILE_PATH,FUNCTION) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		FUNCTION :: function(),
		REASON :: term().

do_line(FILE_PATH,FUNCTION) ->

	{ok,FILE} = file:open(FILE_PATH,read),
	do_line_handler(FILE,FUNCTION).


%% ----------------------------
%% @doc The do_line/2 handler
-spec do_line_handler(FILE_IO,FUNCTION) -> ok | {error,REASON}
	when
		FILE_IO :: io:device(),
		FUNCTION :: function(),
		REASON :: term().

do_line_handler(FILE_IO, FUNCTION) ->

	case io:get_line(FILE_IO,'') of
		eof ->
			file:close(FILE_IO);
		{error,REASON} ->
			{error,REASON};
		LINE ->
			FUNCTION(LINE),
			do_line_handler(FILE_IO,FUNCTION)
	end.


%% ----------------------------
%% @doc Do the defined function in module for the line from file
-spec do_line(FILE_PATH,MODULE,FUNCTION,ARGUMENTS) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		MODULE :: atom(),
		FUNCTION :: atom(),
		ARGUMENTS :: list(),
		REASON :: term().

do_line(FILE_PATH,MODULE,FUNCTION,ARGUMENTS) ->

	{ok,FILE} = file:open(FILE_PATH,read),
	do_line_handler(FILE,MODULE,FUNCTION,ARGUMENTS).


%% ----------------------------
%% @doc The do_line/4 handler
-spec do_line_handler(FILE_IO,MODULE,FUNCTION,ARGUMENTS) -> ok | {error,REASON}
	when
		FILE_IO :: io:device(),
		MODULE :: atom(),
		FUNCTION :: atom(),
		ARGUMENTS :: a_list_of_properties(),
		REASON :: term().

do_line_handler(FILE_IO,MODULE,FUNCTION,ARGUMENTS) ->

	case io:get_line(FILE_IO,'') of
		eof ->
			file:close(FILE_IO);
		{error,Reason} ->
			{error,Reason};
		LINE ->
			apply(MODULE,FUNCTION,lists:append([LINE],ARGUMENTS)),
			do_line_handler(FILE_IO,MODULE,FUNCTION,ARGUMENTS)
	end.


%% ----------------------------
%% @doc Check file existence and it's not directory
-spec existed(FILE_PATH) -> boolean() | dir
	when FILE_PATH :: a_unix_path_string().

existed(FILE_PATH) ->

	case filelib:is_file(FILE_PATH) of
		true ->
			case filelib:is_dir(FILE_PATH) of
				false -> true;
				true -> dir
			end;
		false ->
			false
	end.


%% ----------------------------
%% @doc Create file within checking existence
-spec create(FILE_PATH,DATA) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		DATA :: <<>>,
		REASON :: term() | existed | dir.

create(FILE_PATH,DATA) ->

	case existed(FILE_PATH) of
		false -> create_no_check(FILE_PATH,DATA);
		true -> {error,existed};
		dir -> {error,dir}
	end.


%% ----------------------------
%% @doc Create file with binary data without checking existence
-spec create_no_check(FILE_PATH,DATA) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		DATA :: <<>>,
		REASON :: term().

create_no_check(FILE_PATH,DATA) -> file:write_file(FILE_PATH,DATA).


%% ----------------------------
%% @doc Create empty file within checking existence
-spec create_empty(FILE_PATH) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		REASON :: term().

create_empty(FILE_PATH) ->

	case existed(FILE_PATH) of
		false -> create_empty_no_check(FILE_PATH);
		true -> {error,existed};
		dir -> {error,dir}
	end.


%% ----------------------------
%% @doc Create empty file without checking existence
-spec create_empty_no_check(FILE_PATH) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		REASON :: term().

create_empty_no_check(FILE_PATH) -> create_no_check(FILE_PATH,<<>>).


%% ----------------------------
%% @doc Clear existed file within checking
-spec clear(FILE_PATH) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		REASON :: term() | no_file | dir.

clear(FILE_PATH) ->

	case existed(FILE_PATH) of
		true -> clear_no_check(FILE_PATH);
		false -> {error,no_file};
		dir -> {error,dir}
	end.


%% ----------------------------
%% @doc Clear existed file without checking
-spec clear_no_check(FILE_PATH) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		REASON :: term() | no_file | dir.

clear_no_check(FILE_PATH) ->

	case file:delete(FILE_PATH) of
		ok -> create_empty_no_check(FILE_PATH);
		ERROR_DELETE -> ERROR_DELETE
	end.


%% ----------------------------
%% @doc Delete file within checking existence
-spec delete(FILE_PATH) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		REASON :: term() | no_file | dir.

delete(FILE_PATH) ->

	case existed(FILE_PATH) of
		true -> delete_no_check(FILE_PATH);
		false -> {error,no_file};
		dir -> {error,dir}
	end.


%% ----------------------------
%% @doc Delete file without checking existence
-spec delete_no_check(FILE_PATH) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		REASON :: term().

delete_no_check(FILE_PATH) -> file:delete(FILE_PATH).


%% ----------------------------
%% @doc Add data to the begin of the file within checking existence
-spec add_to_begin(FILE_PATH,DATA) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		DATA :: <<>>,
		REASON :: term() | no_file | dir.

add_to_begin(FILE_PATH,DATA) ->

	case existed(FILE_PATH) of
		true -> add_to_begin_no_check(FILE_PATH,DATA);
		false -> {error,no_file};
		dir -> {error,dir}
	end.


%% ----------------------------
%% @doc Write data to the begin of the file without checking
-spec add_to_begin_no_check(FILE_PATH,DATA) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		DATA :: <<>>,
		REASON :: term().

add_to_begin_no_check(FILE_PATH,DATA) ->

	case file:read_file(FILE_PATH) of
		{ok,EXISTED_DATA} ->
			file:write_file(
				FILE_PATH,
				iolist_to_binary([DATA,EXISTED_DATA])
			);
		{error,REASON} ->
			{error,REASON}
	end.


%% ----------------------------
%% @doc Add data to the end of the file
-spec add_to_end(FILE_PATH,DATA) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		DATA :: <<>>,
		REASON :: term() | no_file | dir.

add_to_end(FILE_PATH,DATA) ->

	case existed(FILE_PATH) of
		true -> add_to_end_no_check(FILE_PATH,DATA);
		false -> {error,no_file};
		dir -> {error,dir}
	end.


%% ----------------------------
%% @doc Add data to the end of the file without checking existence
-spec add_to_end_no_check(FILE_PATH,DATA) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		DATA :: <<>>,
		REASON :: term().

add_to_end_no_check(FILE_PATH,DATA) ->

	file:write_file(FILE_PATH,DATA,[append]).


%% ----------------------------
%% @doc Add string line to the begin of the file
-spec add_line_to_begin(FILE_PATH,STRING) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		STRING :: a_utf_text_string(),
		REASON :: term() | no_file | dir.

add_line_to_begin(FILE_PATH,STRING) ->

	case existed(FILE_PATH) of
		true -> add_line_to_begin_no_check(FILE_PATH,STRING);
		false -> {error,no_file};
		dir -> {error,dir}
	end.


%% ----------------------------
%% @doc Add string line to the begin of the file without no check
-spec add_line_to_begin_no_check(FILE_PATH,STRING) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		STRING :: a_utf_text_string(),
		REASON :: term() | no_file | dir.

add_line_to_begin_no_check(FILE_PATH,STRING) ->

	add_to_begin_no_check(
		FILE_PATH,
		unicode:characters_to_binary(STRING ++ "\n")
	).


%% ----------------------------
%% @doc Add string line to the end of the file
-spec add_line_to_end(FILE_PATH,STRING) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		STRING :: a_utf_text_string(),
		REASON :: term() | no_file | dir.

add_line_to_end(FILE_PATH,STRING) ->

	case existed(FILE_PATH) of
		true -> add_line_to_end_no_check(FILE_PATH,STRING);
		false -> {error,no_file};
		dir -> {error,dir}
	end.


%% ----------------------------
%% @doc Add string line to the end of the file without no check
-spec add_line_to_end_no_check(FILE_PATH,STRING) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		STRING :: a_utf_text_string(),
		REASON :: term() | no_file | dir.

add_line_to_end_no_check(FILE_PATH,STRING) ->

	add_to_end_no_check(
		FILE_PATH,
		unicode:characters_to_binary("\n" ++ STRING)
	).


%% ----------------------------
%% @doc Delete first line within checking file existence
-spec delete_first_line(FILE_PATH) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		REASON :: term() | no_file | dir.

delete_first_line(FILE_PATH) ->

	case existed(FILE_PATH) of
		true -> delete_first_line_no_check(FILE_PATH);
		false -> {error,no_file};
		dir -> {error,dir}
	end.


%% ----------------------------
%% @doc Delete first line without checking file existence
-spec delete_first_line_no_check(FILE_PATH) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		REASON :: term().

delete_first_line_no_check(FILE_PATH) ->

	case file:read_file(FILE_PATH) of
		{ok,EXISTED_CONTENT} ->
			EXISTED_LINES = binary:split(EXISTED_CONTENT,<<"\n">>,[global]),
			NEW_LINES = case EXISTED_LINES of
				[] -> [];
				[_FIRST|LINES] -> LINES
			end,
			file:write_file(
				FILE_PATH,
				list_to_binary(lists:join(<<"\n">>,NEW_LINES))
			);
		ERROR_READ ->
			ERROR_READ
	end.


%% ----------------------------
%% @doc Delete last line within checking file existence
-spec delete_last_line(FILE_PATH) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		REASON :: term() | no_file | dir.

delete_last_line(FILE_PATH) ->

	case existed(FILE_PATH) of
		true -> delete_last_line_no_check(FILE_PATH);
		false -> {error,no_file};
		dir -> {error,dir}
	end.


%% ----------------------------
%% @doc Delete last line without checking file existence
-spec delete_last_line_no_check(FILE_PATH) -> ok | {error,REASON}
	when
		FILE_PATH :: a_unix_path_string(),
		REASON :: term().

delete_last_line_no_check(FILE_PATH) ->

	case file:read_file(FILE_PATH) of
		{ok,EXISTED_CONTENT} ->
			EXISTED_LINES = binary:split(EXISTED_CONTENT,<<"\n">>,[global]),
			NEW_LINES = case EXISTED_LINES of
				[] ->
					[];
				[_FIRST|LINES] ->
					[_FIRST_REVERSED|REVERSED_LINES] = lists:reverse(LINES),
					lists:reverse(REVERSED_LINES)
			end,
			file:write_file(
				FILE_PATH,
				list_to_binary(lists:join(<<"\n">>,NEW_LINES))
			);
		ERROR_READ ->
			ERROR_READ
	end.
