%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 02. Oct 2024 18:43
%%%-------------------------------------------------------------------
-module(a_ssh).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% System includes

%% Application includes
-include("a_includes.hrl").

%% API
-export([

	test/0,

	execute/2,
	is_executable/1

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Execute command on defined server
-spec execute(SERVER,COMMAND) -> OUTPUT
	when
		SERVER :: a_host_name_string(),
		COMMAND :: a_shell_command_string(),
		OUTPUT :: {true,COMMAND_OUTPUT} | {false,{SERVER,COMMAND}},
		COMMAND_OUTPUT :: string().

execute(SERVER,COMMAND) ->

	OS_COMMAND = "ssh " ++ SERVER ++ " '" ++ COMMAND ++ "' 2>&1",
	case is_executable(SERVER) of
		true -> {true,os:cmd(OS_COMMAND)};
		false -> {false,{SERVER,COMMAND}}
	end.


%% ----------------------------
%% @doc Check ability to execute command on defined server
-spec is_executable(SERVER) -> OUTPUT
	when
		SERVER :: a_host_name_string(),
		OUTPUT :: true | false.

is_executable(SERVER) ->

	OS_COMMAND = "ssh " ++ SERVER ++ " 'echo ok' 2>&1",
	case os:cmd(OS_COMMAND) of
		"ok\n" -> true;
		_ -> false
	end.