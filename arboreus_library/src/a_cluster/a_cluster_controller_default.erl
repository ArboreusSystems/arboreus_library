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
	get_nodes_by_type_handler/0

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Return default handler state
-spec handler_state() -> STATE
	when STATE :: #a_cluster_controller_handler_state{}.

handler_state() ->

	#a_cluster_controller_handler_state{
		get_nodes_handler = get_nodes_by_type_handler()
	}.


%% ----------------------------
%% @doc Return for filtering nodes by type
-spec get_nodes_by_type_handler() -> fun().

get_nodes_by_type_handler() ->

	fun([IN_TYPE],IN_NODES) when is_list(IN_NODES) ->
		lists:filter(
			fun(NODE_DATA) -> NODE_DATA#a_cluster_node_data.type =:= IN_TYPE end,
			IN_NODES
		)
	end.


