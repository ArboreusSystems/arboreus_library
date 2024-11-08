%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 02. Jul 2024 07:06
%%%-------------------------------------------------------------------
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% System includes

%% Application includes
-include("../a_includes.hrl").

-ifndef(A_RECORDS_CLUSTER).
-define(A_RECORDS_CLUSTER,1).

-record(a_cluster_node_data,{

	id = <<"NoDefinedID"/utf8>> :: a_id_32(),
	type = "NoDefinedType" :: any(),
	name = "NoDefinedName" :: a_utf_text_string(),
	server = "NoDefinedServer" :: a_host_name_string(),
	propperties = [] :: proplists:proplist()
}).

-record(a_cluster_controller_handler_state,{

	db :: pid(),
	monitor :: pid(),
	get_nodes_by_handler = fun(IN_PROPERTIES,IN_ALL_NODES) ->
		{error,{no_handler,{get_nodes_by_handler,IN_PROPERTIES,IN_ALL_NODES}}}
	end :: fun()
}).

-record(a_cluster_controller_db_state,{

	handler :: pid(),
	monitor :: pid(),
	ets_nodes :: reference()
}).

-record(a_cluster_controller_monitor_state,{

	handler :: pid(),
	db :: pid()
}).

-record(a_cluster_connector_handler_state,{

	main_controller = 'none@noserver.nodomain' :: a_node_name_atom(),
	data = #a_cluster_node_data{} :: #a_cluster_node_data{},
	get_nodes_by_type_handler = fun(IN_TYPE,IN_MAIN_CONTROLLER) ->
		{error,{no_handler,{get_node_by_type_handler,IN_TYPE,IN_MAIN_CONTROLLER}}}
	end,
	add_node_handler = fun(IN_NODE_DATA,IN_MAIN_CONTROLLER) ->
		{error,{no_handler,{add_node_handler,IN_NODE_DATA,IN_MAIN_CONTROLLER}}}
	end :: fun(),
	delete_node_by_id_handler = fun(IN_NODE_ID,IN_MAIN_CONTROLLER) ->
		{error,{no_handler,{delete_node_by_id_handler,IN_NODE_ID,IN_MAIN_CONTROLLER}}}
	end :: fun()
}).

-record(a_cluster_controller_properties,{

	handler_state = #a_cluster_controller_handler_state{}
	:: #a_cluster_controller_handler_state{},
	monitor_state = #a_cluster_controller_monitor_state{}
	:: #a_cluster_controller_monitor_state{},
	db_state = #a_cluster_controller_db_state{}
	:: #a_cluster_controller_db_state{}
}).

-endif. %% A_RECORDS_CLUSTER