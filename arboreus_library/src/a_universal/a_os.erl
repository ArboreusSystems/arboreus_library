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
	ls_filtered/2,ls_filtered_no_check/2,

	ps/1,
	ps_for_pid/2,
	ps_get_pid_memory/1,ps_get_pid_memory_physical/1,ps_get_pid_memory_virtual/1,

	df/1,
	df_for_disk/1,df_for_disk_proplist/1

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


%% ----------------------------
%% @doc Return result of 'ps' command
-spec ps(PARAMETERS) -> RESULT
	when
		PARAMETERS :: a_utf_text_string(),
		RESULT :: list().

ps(PARAMETERS) ->

	string:tokens(os:cmd(string:concat("ps ",PARAMETERS)),"\n").


%% ----------------------------
%% @doc Return result of 'ps' shell command for defined OS PID
-spec ps_for_pid(PARAMETERS,PID_OS) -> RESULT
	when
		PARAMETERS :: a_utf_text_string(),
		PID_OS :: a_utf_text_string() | pos_integer(),
		RESULT :: list().

ps_for_pid(PARAMETERS,PID_OS) when is_integer(PID_OS)->

	ps_for_pid(PARAMETERS,"" ++ integer_to_list(PID_OS));

ps_for_pid(PARAMETERS,PID_OS) when is_list(PARAMETERS),is_list(PID_OS) ->

	ps(string:concat(PARAMETERS,string:concat(" -p ",PID_OS))).


%% ----------------------------
%% @doc Return process memory usage by PID
-spec ps_get_pid_memory(PID_OS) -> {ok,PHYSICAL_MEMORY_KB,VIRTUAL_MEMORY_KB} | {error,REASON}
	when
		PID_OS :: pos_integer() | a_utf_text_string(),
		PHYSICAL_MEMORY_KB :: pos_integer(),
		VIRTUAL_MEMORY_KB :: pos_integer(),
		REASON :: term().

ps_get_pid_memory(PID_OS) ->

	case ps_for_pid("-x -o rss,vsz",PID_OS) of
		[_,MEMORY_VALUE_STRING] ->
			[RSS_STRING_KB,VSZ_STRING_KB] = string:tokens(MEMORY_VALUE_STRING," "),
			{ok,list_to_integer(RSS_STRING_KB),list_to_integer(VSZ_STRING_KB)};
		[ERROR_RESULT] ->
			{error,{ERROR_RESULT}}
	end.


%% ----------------------------
%% @doc Return physical memory measure used by process by PID
-spec ps_get_pid_memory_physical(PID_OS) -> {ok,PHYSICAL_MEMORY_KB} | {error,REASON}
	when
		PID_OS :: pos_integer() | a_utf_text_string(),
		PHYSICAL_MEMORY_KB :: pos_integer(),
		REASON :: term().

ps_get_pid_memory_physical(PID_OS) ->

	case ps_for_pid("-x -o rss",PID_OS) of
		[_,MEMORY_VALUE_STRING] ->
			{ok,list_to_integer(string:trim(MEMORY_VALUE_STRING))};
		[ERROR_RESULT] ->
			{error,{ERROR_RESULT}}
	end.


%% ----------------------------
%% @doc Return virtual memory measure used by process by PID
-spec ps_get_pid_memory_virtual(PID_OS) -> {ok,VIRTUAL_MEMORY_KB} | {error,REASON}
	when
		PID_OS :: pos_integer() | a_utf_text_string(),
		VIRTUAL_MEMORY_KB :: pos_integer(),
		REASON :: term().

ps_get_pid_memory_virtual(PID_OS) ->

	case ps_for_pid("-x -o vsz",PID_OS) of
		[_,MEMORY_VALUE_STRING] ->
			{ok,list_to_integer(string:trim(MEMORY_VALUE_STRING))};
		[ERROR_RESULT] ->
			{error,{ERROR_RESULT}}
	end.


%% ----------------------------
%% @doc Return result of 'df' command execution
-spec df(PARAMETERS) -> a_utf_text_string()
	when PARAMETERS :: a_utf_text_string().

df(PARAMETERS) ->

	os:cmd(string:concat("df ",PARAMETERS)).


%% ----------------------------
%% @doc Return result of 'df' command for exact path
-spec df_for_disk(PATH) -> list()
	when PATH :: a_unix_path_string().

df_for_disk(PATH) ->

	[_|[DISK_INFO]] = string:tokens(df("-li " ++ PATH),"\n"),
	string:tokens(DISK_INFO," ").


%% ----------------------------
%% @doc Return result of 'df' command for exact path in proplist
-spec df_for_disk_proplist(DISK_PATH) -> proplists:proplist()
	when DISK_PATH :: a_unix_path_string().

df_for_disk_proplist(DISK_PATH) ->

	[FILESYSTEM,BLOCKS,USED,AVAILABLE,CAPACITY,IUSED,IFREE,IPERCENT,MOUNTED] = df_for_disk(DISK_PATH),
	[
		{filesystem,FILESYSTEM},
		{blocks,BLOCKS},
		{used,USED},
		{available,AVAILABLE},
		{capacity,CAPACITY},
		{iused,IUSED},
		{ifree,IFREE},
		{ipercent,IPERCENT},
		{mounted,MOUNTED}
	].