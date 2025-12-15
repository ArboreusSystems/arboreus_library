%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2025, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 15. Dec 2025 20:12
%%%-------------------------------------------------------------------
-module(a_os).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% System includes

%% Application includes
-include("a_includes.hrl").

%% API
-export([

	test/0,

	ls/1,ls_no_check/1,
	ls_filtered/2,ls_filtered_no_check/2

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Return result of 'ls' system command within error checking
-spec ls(PATH) -> {ok,LIST_OF_FILES} | {error,REASON}
	when
		PATH :: a_unix_path_string(),
		LIST_OF_FILES :: [FILE_NAME],
		FILE_NAME :: a_file_name_string(),
		REASON :: string().

ls(PATH) ->

	case ls_no_check(PATH) of
		[SINGLE_RESULT] ->
			case re:run(SINGLE_RESULT,"^ls:*") of
				{match,_CAPTURED} ->
					{error,SINGLE_RESULT};
				nomatch ->
					{ok,[SINGLE_RESULT]}
			end;
		RESULT ->
			{ok,RESULT}
	end.


%% ----------------------------
%% @doc Return result of 'ls' system command
-spec ls_no_check(PATH) -> LIST_OF_FILES
	when
		PATH :: a_unix_path_string(),
		LIST_OF_FILES :: [FILE_NAME],
		FILE_NAME :: a_file_name_string().

ls_no_check(PATH) ->

	string:tokens(os:cmd("ls " ++ PATH),"\n").


%% ----------------------------
%% @doc Return filtered by 'grep' result of 'ls' system command within error checking
-spec ls_filtered(PATH,FILTER) -> {ok,LIST_OF_FILES} | {error,REASON}
	when
		PATH :: a_unix_path_string(),
		FILTER :: a_utf_text_string(),
		LIST_OF_FILES :: [FILE_NAME],
		FILE_NAME :: a_file_name_string(),
		REASON :: string().

ls_filtered(PATH,FILTER) ->

	case ls_filtered_no_check(PATH,FILTER) of
		[SINGLE_RESULT] ->
			case re:run(SINGLE_RESULT,"^ls:*") of
				{match,_CAPTURED} ->
					{error,SINGLE_RESULT};
				nomatch ->
					{ok,[SINGLE_RESULT]}
			end;
		RESULT ->
			{ok,RESULT}
	end.


%% ----------------------------
%% @doc Return filtered by 'grep' result of 'ls' system command
-spec ls_filtered_no_check(PATH,FILTER) -> LIST_OF_FILES
	when
		PATH :: a_unix_path_string(),
		FILTER :: a_utf_text_string(),
		LIST_OF_FILES :: [FILE_NAME],
		FILE_NAME :: a_file_name_string().

ls_filtered_no_check(PATH,FILTER) ->

	string:tokens(os:cmd("ls " ++ PATH ++ " | grep " ++ FILTER),"\n").