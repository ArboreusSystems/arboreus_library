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

%% System include
-include("../data_models/types/types_general.hrl").

%% API
-export([
	test/0,
	do_line/2,do_line/4
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Do the function for the line from requested file
-spec do_line(File_path,Function) -> ok | {error,_Reason}
	when
	File_path :: unix_path_string(),
	Function :: function().

do_line(File_path,Function) ->
	{ok,File} = file:open(File_path,read),
	do_line_handler(File,Function).


%% ----------------------------
%% @doc The do_line/2 handler
-spec do_line_handler(File,Function) -> ok | {error,_Reason}
	when
	File :: io:device(),
	Function :: function().

do_line_handler(File,Function) ->
	case io:get_line(File,'') of
		eof -> file:close(File);
		{error,Reason} -> {error,Reason};
		Line ->
			Function(Line),
			do_line_handler(File,Function)
	end.


%% ----------------------------
%% @doc Do the defined function in module for the line from file
-spec do_line(File_path,Module,Function,Arguments) -> ok | {error,_Reason}
	when
	File_path :: unix_path_string(),
	Module :: atom(),
	Function :: atom(),
	Arguments :: list().

do_line(File_path,Module,Function,Arguments) ->
	{ok,File} = file:open(File_path,read),
	do_line_handler(File,Module,Function,Arguments).


%% ----------------------------
%% @doc The do_line/4 handler
-spec do_line_handler(File,Module,Function,Arguments) -> ok | {error,_Reason}
	when
	File :: io:device(),
	Module :: atom(),
	Function :: atom(),
	Arguments :: list_of_properties().

do_line_handler(File,Module,Function,Arguments) ->
	case io:get_line(File,'') of
		eof -> file:close(File);
		{error,Reason} -> {error,Reason};
		Line ->
			apply(Module,Function,lists:append([Line],Arguments)),
			do_line_handler(File,Module,Function,Arguments)
	end.