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

test() -> ok.


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