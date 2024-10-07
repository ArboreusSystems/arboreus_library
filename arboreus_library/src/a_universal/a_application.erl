%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2024 23:28
%%%-------------------------------------------------------------------
-module(a_application).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% System includes

%% Application includes
-include("a_includes.hrl").

%% API
-export([

	test/0,

	is_started/1,is_started/2,
	stop/1,stop/2,
	stop_by/4,stop_by/5,
	stop_by_function/2,stop_by_function/3

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Check if application in list of started applications on current node
-spec is_started(APPLICATION_NAME) -> OUTPUT
	when
		APPLICATION_NAME :: string() | atom(),
		OUTPUT :: {true,APPLICATION_NAME} | {false,APPLICATION_NAME}.

is_started(APPLICATION_NAME) when is_list(APPLICATION_NAME) ->

	is_started(APPLICATION_NAME);

is_started(APPLICATION_NAME) when is_atom(APPLICATION_NAME) ->

	case lists:keyfind(APPLICATION_NAME,1,application:which_applications()) of
		{APPLICATION_NAME,_DESCRIPTION,_VERSION} -> {true,APPLICATION_NAME};
		_ -> {false,APPLICATION_NAME}
	end.


%% ----------------------------
%% @doc Check if application in list of started applications on current node
-spec is_started(NODE_NAME,APPLICATION_NAME) -> OUTPUT
	when
		NODE_NAME :: a_node_name_string() | atom(),
		APPLICATION_NAME :: string() | atom(),
		OUTPUT :: {true,APPLICATION_NAME} | {false,APPLICATION_NAME} | {badrpc,nodedown}.

is_started(NODE_NAME,APPLICATION_NAME) when is_list(APPLICATION_NAME) ->

	is_started(NODE_NAME,list_to_atom(APPLICATION_NAME));

is_started(NODE_NAME,APPLICATION_NAME) when is_atom(APPLICATION_NAME) ->

	a_rpc:async_call(NODE_NAME,a_application,is_started,[APPLICATION_NAME]).


%% ----------------------------
%% @doc Stop application if started
-spec stop(APPLICATION_NAME) -> OUTPUT
	when
		APPLICATION_NAME :: list() | atom(),
		OUTPUT :: ok | {error,REASON},
		REASON :: term().

stop(APPLICATION_NAME) ->

	case is_started(APPLICATION_NAME) of
		{true,APPLICATION_NAME} -> application:stop(APPLICATION_NAME);
		{false,APPLICATION_NAME} -> ok
	end.


%% ----------------------------
%% @doc Stop application if started on defined node
-spec stop(NODE_NAME,APPLICATION_NAME) -> OUTPUT
	when
		NODE_NAME :: node(),
		APPLICATION_NAME :: list() | atom(),
		OUTPUT :: ok | {error,REASON} | {badrpc,nodedown},
		REASON :: term().

stop(NODE_NAME,APPLICATION_NAME) ->

	a_rpc:async_call(NODE_NAME,a_application,stop,[APPLICATION_NAME]).


%% ----------------------------
%% @doc Stop application if started by defined module and function within parameters
-spec stop_by(APPLICATION_NAME,MODULE,FUNCTION,PARAMETERS) -> OUTPUT
	when
		APPLICATION_NAME :: list() | atom(),
		MODULE :: atom(),
		FUNCTION :: atom(),
		PARAMETERS :: [any()],
		OUTPUT :: ok | any().

stop_by(APPLICATION_NAME,MODULE,FUNCTION,PARAMETERS) ->

	case is_started(APPLICATION_NAME) of
		{true,APPLICATION_NAME} -> erlang:apply(MODULE,FUNCTION,PARAMETERS);
		{false,APPLICATION_NAME} -> ok
	end.


%% ----------------------------
%% @doc Stop application if started by defined module and function within parameters
%% on defined node
-spec stop_by(NODE_NAME,APPLICATION_NAME,MODULE,FUNCTION,PARAMETERS) -> OUTPUT
	when
		NODE_NAME :: node(),
		APPLICATION_NAME :: list() | atom(),
		MODULE :: atom(),
		FUNCTION :: atom(),
		PARAMETERS :: [any()],
		OUTPUT :: ok | any().

stop_by(NODE_NAME,APPLICATION_NAME,MODULE,FUNCTION,PARAMETERS) ->

	a_rpc:async_call(
		NODE_NAME,
		a_application,stop_by,[APPLICATION_NAME,MODULE,FUNCTION,PARAMETERS]
	).


%% ----------------------------
%% @doc Stop application if started by defined function
-spec stop_by_function(APPLICATION_NAME,FUNCTION) -> OUTPUT
	when
		APPLICATION_NAME :: list() | atom(),
		FUNCTION :: fun((IN_APPLICATION_NAME) -> any()),
		IN_APPLICATION_NAME :: APPLICATION_NAME,
		OUTPUT :: ok | any().

stop_by_function(APPLICATION_NAME,FUNCTION) ->

	case is_started(APPLICATION_NAME) of
		{true,APPLICATION_NAME} -> FUNCTION(APPLICATION_NAME);
		{false,APPLICATION_NAME} -> ok
	end.


%% ----------------------------
%% @doc Stop application if started on defined node by defined function
-spec stop_by_function(NODE_NAME,APPLICATION_NAME,FUNCTION) -> OUTPUT
	when
		NODE_NAME :: node(),
		APPLICATION_NAME :: list() | atom(),
		FUNCTION :: fun((IN_APPLICATION_NAME) -> any()),
		IN_APPLICATION_NAME :: APPLICATION_NAME,
		OUTPUT :: ok | any() | {badrpc,nodedown}.

stop_by_function(NODE_NAME,APPLICATION_NAME,FUNCTION) ->

	a_rpc:async_call(NODE_NAME,a_application,stop_by_function,[APPLICATION_NAME,FUNCTION]).