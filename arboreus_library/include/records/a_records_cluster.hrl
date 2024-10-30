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
	name = "NoDefinedName" :: a_utf_text_string(),
	server = "NoDefinedServer" :: a_host_name_string(),
	propperties = [] :: proplists:proplist()
}).

-record(a_cluster_controller_properties,{

}).

-record(a_cluster_controller_handler_state,{
	db :: pid(),
	monitor :: pid(),
	get_node_handler :: fun()
}).

-record(a_cluster_controller_db_state,{
	handler :: pid(),
	monitor :: pid()
}).

-record(a_cluster_controller_monitor_state,{
	handler :: pid(),
	db :: pid()
}).

-record(a_cluster_connector_properties,{

}).

-record(a_cluster_connector_handler_state,{

}).

-endif. %% A_RECORDS_CLUSTER