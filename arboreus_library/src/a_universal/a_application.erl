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

	is_started/1,is_started/2

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