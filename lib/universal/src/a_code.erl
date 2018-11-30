%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Add-on for standard code module
%%%
%%% @end
%%% Created : 08. Янв. 2018 20:07
%%%-------------------------------------------------------------------
-module(a_code).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% System include
-include("../data_models/types/types_general.hrl").

%% API
-export([
	test/0,
	is_module/1,
	is_path/1,
	l/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_code) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Module_name = test_module,
	Module_binary = <<
		("\-module(")/utf8,(atom_to_binary(Module_name,utf8))/binary,(").\n")/utf8,
		("-export([test/0]).\n")/utf8,
		("test() -> ok.\n")/utf8
	>>,
	{ok,CWD} = file:get_cwd(),
	Module_path_erl = lists:concat([CWD,"/",atom_to_list(Module_name),".erl"]),
	Module_path_beam = lists:concat([CWD,"/",atom_to_list(Module_name),".beam"]),
	ok = file:write_file(Module_path_erl,Module_binary),
	{ok,Module_name} = compile:file(Module_path_erl),
	ok = Module_name:test(),
	{ok,_} = l([Module_name]),
	io:format("DONE! Function l/1 test passed.~n"),
	{Module_name,Module_path_beam} = is_module(Module_name),
	false = is_module(no_module),
	io:format("DONE! Function is_module/1 test passed.~n"),
	true = is_path(CWD),
	false = is_path("wrong_path"),
	io:format("DONE! Function is_path/1 test passed.~n"),
	ok = file:delete(Module_path_erl),
	ok = file:delete(Module_path_beam),
	false = code:purge(Module_name),
	true = code:delete(Module_name),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_code) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.
	


%% ----------------------------
%% @doc Wrapper for l_handler/2
-spec l(Modules) -> {ok,Output}
	when
	Modules :: [Module],
	Output :: [{Module,code:load_ret()}],
	Module :: atom().

l(Modules) -> l_handler(Modules,[]).


%% ----------------------------
%% @doc Reload modules from list of module's names
-spec l_handler(Modules,Output) -> {ok,Output}
	when
	Modules :: [Module],
	Output :: [{Module,code:load_ret()}],
	Module :: atom().

l_handler([],Output) -> {ok,Output};
l_handler([Module|Modules],Output) ->
	l_handler(
		Modules,
		lists:append(Output,[{Module,c:l(Module)}])
	).
	

%% ----------------------------
%% @doc Check the module for presence in the Erlang environment.
-spec is_module(Module) -> {Module,Path} | false
	when
	Module :: module(),
	Path :: unix_path_string().

is_module(Module) ->
	lists:keyfind(Module,1,code:all_loaded()).


%% ----------------------------
%% @doc Check the path for presence in the Erlang environment.
-spec is_path(Path::unix_path_string()) -> boolean().

is_path(Path) ->
	lists:member(Path,code:get_path()).