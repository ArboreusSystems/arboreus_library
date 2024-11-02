%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 21. Jun 2024 12:30
%%%-------------------------------------------------------------------
-module(a_cluster_connector).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% System includes

%% Application includes
-include("../../include/a_includes.hrl").

%% API
-export([

	test/0,

	pid_handler/1

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Return Cluster Connector handler PID
-spec pid_handler(SUPERVISOR_PID) -> {ok,HANDLER_PID} | {error,REASON}
	when
		SUPERVISOR_PID :: pid(),
		HANDLER_PID :: pid(),
		REASON :: term().

pid_handler(SUPERVISOR_PID) ->

	case a_otp_supervisor:find_child(
		by_id,SUPERVISOR_PID,?A_ID_CLUSTER_CONNECTOR_HANDLER
	) of
		{?A_ID_CLUSTER_CONNECTOR_HANDLER,HANDLER_PID,_TYPE,_MODULES} ->
			{ok,HANDLER_PID};
		false ->
			{error,no_cluster_controller_handler}
	end.