%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 21. Jun 2024 12:29
%%%-------------------------------------------------------------------
-module(a_cluster_controller).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% System includes

%% Application includes
-include("../../include/a_includes.hrl").

%% API
-export([

	test/0,

	pid_db/1,
	pid_handler/1,
	pid_monitor/1,

	node_name/1,
	get_nodes_by_handler/2,
	define_get_nodes_handler/2,
	get_all_nodes/1,
	add_node/2

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Return Cluster Controller db PID
-spec pid_db(SUPERVISOR_PID) -> {ok,DB_PID} | {error,REASON}
	when
		SUPERVISOR_PID :: pid(),
		DB_PID :: pid(),
		REASON :: term().

pid_db(SUPERVISOR_PID) ->

	case a_otp_supervisor:find_child(
		by_id,SUPERVISOR_PID,?A_ID_CLUSTER_CONTROLLER_DB
	) of
		{?A_ID_CLUSTER_CONTROLLER_DB,DB_PID,_TYPE,_MODULES} ->
			{ok,DB_PID};
		false ->
			{error,no_cluster_controller_db}
	end.


%% ----------------------------
%% @doc Return Cluster Controller handler PID
-spec pid_handler(SUPERVISOR_PID) -> {ok,HANDLER_PID} | {error,REASON}
	when
		SUPERVISOR_PID :: pid(),
		HANDLER_PID :: pid(),
		REASON :: term().

pid_handler(SUPERVISOR_PID) ->

	case a_otp_supervisor:find_child(
		by_id,SUPERVISOR_PID,?A_ID_CLUSTER_CONTROLLER_HANDLER
	) of
		{?A_ID_CLUSTER_CONTROLLER_HANDLER,HANDLER_PID,_TYPE,_MODULES} ->
			{ok,HANDLER_PID};
		false ->
			{error,no_cluster_controller_handler}
	end.


%% ----------------------------
%% @doc Return Cluster Controller monitor PID
-spec pid_monitor(SUPERVISOR_PID) -> {ok,MONITOR_PID} | {error,REASON}
	when
		SUPERVISOR_PID :: pid(),
		MONITOR_PID :: pid(),
		REASON :: term().

pid_monitor(SUPERVISOR_PID) ->

	case a_otp_supervisor:find_child(
		by_id,SUPERVISOR_PID,?A_ID_CLUSTER_CONTROLLER_MONITOR
	) of
		{?A_ID_CLUSTER_CONTROLLER_MONITOR,MONITOR_PID,_TYPE,_MODULES} ->
			{ok,MONITOR_PID};
		false ->
			{error,no_cluster_controller_monitor}
	end.


%% ----------------------------
%% @doc Return Cluster Controller node name
-spec node_name(SUPERVISOR_PID) -> {ok,NODE_NAME}
	when
		SUPERVISOR_PID :: pid(),
		NODE_NAME :: a_node_name_atom().

node_name(SUPERVISOR_PID) ->

	{ok,HANDLER_PID} = pid_handler(SUPERVISOR_PID),
	gen_server:call(HANDLER_PID,node_name).


%% ----------------------------
%% @doc Return list of nodes filtered by handler
-spec get_nodes_by_handler(PARAMETERS,SUPERVISOR_PID) -> NODES
	when
		PARAMETERS :: [any()],
		SUPERVISOR_PID :: pid(),
		NODES :: [#a_cluster_node_data{}].

get_nodes_by_handler(PARAMETERS,SUPERVISOR_PID) ->

	{ok,HANDLER_PID} = pid_handler(SUPERVISOR_PID),
	gen_server:call(HANDLER_PID,{get_nodes_by_handler,PARAMETERS}).


%% ----------------------------
%% @doc Define get nodes handler
-spec define_get_nodes_handler(GET_NODE_HANDLER,SUPERVISOR_PID) -> ok | {error,REASON}
	when
		GET_NODE_HANDLER :: fun(),
		SUPERVISOR_PID :: pid(),
		REASON :: term().

define_get_nodes_handler(GET_NODE_HANDLER,SUPERVISOR_PID) ->

	{ok,HANDLER_PID} = pid_handler(SUPERVISOR_PID),
	gen_server:call(HANDLER_PID,{define_get_nodes_handler,GET_NODE_HANDLER}).


%% ----------------------------
%% @doc Return list of all registered nodes
-spec get_all_nodes(SUPERVISOR_PID) -> {ok,NODES}
	when
		SUPERVISOR_PID :: pid(),
		NODES :: [#a_cluster_node_data{}].

get_all_nodes(SUPERVISOR_PID) ->

	{ok,DB_PID} = pid_db(SUPERVISOR_PID),
	gen_server:call(DB_PID,get_all_nodes).


%% ----------------------------
%% @doc Add node data to Cluster Connector
-spec add_node(NODE_DATA,SUPERVISOR_PID) -> {ok,NODE_DATA} | {error,REASON}
	when
		NODE_DATA :: #a_cluster_node_data{},
		SUPERVISOR_PID :: pid(),
		REASON :: term().

add_node(NODE_DATA,SUPERVISOR_PID) ->

	{ok,DB_PID} = pid_db(SUPERVISOR_PID),
	gen_server:call(DB_PID,{add_node,NODE_DATA}).