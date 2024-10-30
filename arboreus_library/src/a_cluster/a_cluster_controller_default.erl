%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 30. Oct 2024 20:58
%%%-------------------------------------------------------------------
-module(a_cluster_controller_default).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% System includes

%% Application includes
-include("../../include/a_includes.hrl").

%% API
-export([

	test/0,

	handler_state/0,
	get_node_handler/0

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc

handler_state() ->

	#a_cluster_controller_handler_state{
		get_node_handler = get_node_handler()
	}.


%% ----------------------------
%% @doc

get_node_handler() ->

	fun(IN_TYPE,IN_NODE_LIST) when is_list(IN_NODE_LIST) ->
		case proplists:get_value(IN_TYPE,IN_NODE_LIST) of
			[] -> no_node;
			undefined -> no_type;
			[NODE_PROPERTY|_NODE_PROPERTIES] -> NODE_PROPERTY
		end
	end.


