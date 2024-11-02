%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 31. Oct 2024 09:53
%%%-------------------------------------------------------------------
-module(a_cluster_controller_db).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% System includes

%% Application includes
-include("../../include/a_includes.hrl").

%% API
-export([

	test/0,

	init/0,
	add/2,
	select_by_id/2

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Create ETS table for storing nodes data
-spec init() -> {ok,TABLE_REFERENCE} | {error,REASON}
	when
		TABLE_REFERENCE :: reference(),
		REASON :: term().

init() ->

	case ets:new(
		?A_ID_CLUSTER_CONTROLLER_DB_TABLE_NODES,[
			ordered_set,
			private,
			{keypos,#a_cluster_node_data.id}
		]
	) of
		TABLE_REFERENCE when is_reference(TABLE_REFERENCE) ->
			{ok,TABLE_REFERENCE};
		{error,REASON} ->
			{error,REASON}
	end.


%% ----------------------------
%% @doc Add node data to Cluster Controller
-spec add(NODE_DATA,TABLE_REFERENCE) ->
	{ok,NODE_DATA} | {already_added,ALREADY_ADDED_NODE_DATA} | {error,REASON}
	when
		NODE_DATA :: #a_cluster_node_data{},
		ALREADY_ADDED_NODE_DATA :: #a_cluster_node_data{},
		TABLE_REFERENCE :: reference(),
		REASON :: term().

add(NODE_DATA,TABLE_REFERENCE) ->

	case select_by_id(
		NODE_DATA#a_cluster_node_data.id,TABLE_REFERENCE
	) of
		[] ->
			case ets:insert(TABLE_REFERENCE,NODE_DATA) of
				true -> {ok,NODE_DATA};
				_ -> {error,not_inserted}
			end;
		[ALREADY_ADDED_NODE_DATA] ->
			{already_added,ALREADY_ADDED_NODE_DATA}
	end.


%% ----------------------------
%% @doc Select node data record by ID
-spec select_by_id(NODE_ID,TABLE_REFERENCE) -> [] | [NODE_DATA]
	when
		NODE_ID :: a_id_32(),
		TABLE_REFERENCE :: reference(),
		NODE_DATA :: #a_cluster_node_data{}.

select_by_id(NODE_ID,TABLE_REFERENCE) ->

	ets:select(TABLE_REFERENCE,[{
		{a_cluster_node_data,'$1','$2','$3','$4','$5'},
		[{'=:=','$1',NODE_ID}],
		[{{a_cluster_node_data,'$1','$2','$3','$4','$5'}}]
	}]).
