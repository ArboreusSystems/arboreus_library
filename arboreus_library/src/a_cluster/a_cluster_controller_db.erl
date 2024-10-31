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

	init/0

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