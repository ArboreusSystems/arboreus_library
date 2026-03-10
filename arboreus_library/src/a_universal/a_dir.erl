%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2026, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2026 12:52
%%%-------------------------------------------------------------------
-module(a_dir).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% System includes

%% Application includes
-include("a_includes.hrl").

%% API
-export([

	test/0,

	list/1,
	list_dirs/1,list_dirs/2,
	list_files/1,list_files/2

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Return list of files and directories
-spec list(PATH) -> {ok,FILENAMES} | {error,REASON}
	when
		PATH :: a_unix_path_string(),
		FILENAMES :: [a_unix_path_string()],
		REASON :: term().

list(PATH) -> file:list_dir(PATH).


%% ----------------------------
%% @doc Return list of directories only
-spec list_dirs(PATH) -> {ok,DIRS} | {error,REASON}
	when
		PATH :: a_unix_path_string(),
		DIRS :: [a_unix_path_string()],
		REASON :: term().

list_dirs(PATH) -> list_dirs(PATH,names).


%% ----------------------------
%% @doc Return list of directories only within defined output type
-spec list_dirs(PATH,OUTPUT_TYPE) -> {ok,DIR_PATHS} | {error,REASON}
	when
		PATH :: a_unix_path_string(),
		OUTPUT_TYPE :: names | paths,
		DIR_PATHS :: [DIR_PATH],
		DIR_PATH :: a_unix_path_string(),
		REASON :: term().

list_dirs(PATH,OUTPUT_TYPE) ->

	case list(PATH) of
		{ok,FILENAMES} -> list_dirs_handler(PATH,FILENAMES,OUTPUT_TYPE,[]);
		{error,REASON} -> {error,REASON}
	end.


%% ----------------------------
%% @doc Handler for list_dirs/1 function
-spec list_dirs_handler(PATH,DIR_PATHS,OUTPUT_TYPE,OUTPUT) -> {ok,OUTPUT}
	when
		PATH :: a_unix_path_string(),
		DIR_PATHS :: [DIR_PATH],
		DIR_PATH :: a_unix_path_string(),
		OUTPUT_TYPE :: names | paths,
		OUTPUT :: [DIR_PATH].

list_dirs_handler(_PATH,[],_OUTPUT_TYPE,OUTPUT) -> {ok,OUTPUT};

list_dirs_handler(PATH,[DIR_NAME|DIRS],OUTPUT_TYPE,OUTPUT) ->

	DIR_PATH = PATH ++ "/" ++ DIR_NAME,
	case filelib:is_dir(DIR_PATH) of
		true ->
			case OUTPUT_TYPE of
				names ->
					list_dirs_handler(
						PATH,DIRS,OUTPUT_TYPE,lists:append(OUTPUT,[DIR_NAME])
					);
				paths ->
					list_dirs_handler(
						PATH,DIRS,OUTPUT_TYPE,lists:append(OUTPUT,[DIR_PATH])
					)
			end;
		false ->
			list_dirs_handler(
				PATH,DIRS,OUTPUT_TYPE,OUTPUT
			)
	end.


%% ----------------------------
%% @doc Return list of files only
-spec list_files(PATH) -> {ok,FILE_PATHS} | {error,REASON}
	when
		PATH :: a_unix_path_string(),
		FILE_PATHS :: [FILE_PATH],
		FILE_PATH :: a_unix_path_string(),
		REASON :: term().

list_files(PATH) -> list_files(PATH,names).


%% ----------------------------
%% @doc Return list of files only within defined output type
-spec list_files(PATH,OUTPUT_TYPE) -> {ok,FILE_PATHS} | {error,REASON}
	when
		PATH :: a_unix_path_string(),
		OUTPUT_TYPE :: names | paths,
		FILE_PATHS :: [FILE_PATH],
		FILE_PATH :: a_unix_path_string(),
		REASON :: term().

list_files(PATH,OUTPUT_TYPE) ->

	case list(PATH) of
		{ok,FILE_PATHS} -> list_files_handler(PATH,FILE_PATHS,OUTPUT_TYPE,[]);
		{error,REASON} -> {error,REASON}
	end.


%% ----------------------------
%% @doc Handler for list_files/1 function
-spec list_files_handler(PATH,FILE_PATHS,OUTPUT_TYPE,OUTPUT) -> {ok,OUTPUT}
	when
		PATH :: a_unix_path_string(),
		FILE_PATHS :: [FILE_PATH],
		FILE_PATH :: a_unix_path_string(),
		OUTPUT_TYPE :: names | paths,
		OUTPUT :: [FILE_PATH].

list_files_handler(_PATH,[],_OUTPUT_TYPE,OUTPUT) -> {ok,OUTPUT};

list_files_handler(PATH,[FILE_NAME|FILE_NAMES],OUTPUT_TYPE,OUTPUT) ->

	FILE_PATH = PATH ++ "/" ++ FILE_NAME,
	case filelib:is_dir(FILE_PATH) of
		false ->
			case OUTPUT_TYPE of
				names ->
					list_files_handler(
						PATH,FILE_NAMES,OUTPUT_TYPE,lists:append(OUTPUT,[FILE_NAME])
					);
				paths ->
					list_files_handler(
						PATH,FILE_NAMES,OUTPUT_TYPE,lists:append(OUTPUT,[FILE_PATH])
					)
			end;
		true ->
			list_files_handler(
				PATH,FILE_NAMES,OUTPUT_TYPE,OUTPUT
			)
	end.
