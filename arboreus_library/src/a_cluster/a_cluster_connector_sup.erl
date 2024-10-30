%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 21. Jun 2024 12:30
%%%-------------------------------------------------------------------
-module(a_cluster_connector_sup).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").
-behaviour(supervisor).

%% System includes

%% Application includes
-include("../../include/a_includes.hrl").

%% API
-export([

	test/0,

	init/1,
	start_link/0,start_link/1

]).

%% Definitions
-define(SERVER, ?MODULE).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc
-spec start_link() -> {'ok', PID} | 'ignore' | {'error', REASON}
	when
		PID :: pid(),
		REASON :: term().

start_link() ->

	supervisor:start_link({local,?SERVER},?MODULE,[
		#a_cluster_connector_properties{}
	]).


%% ----------------------------
%% @doc
-spec start_link(CLUSTER_CONNECTOR_PROPERTIES) -> {'ok', PID} | 'ignore' | {'error', REASON}
	when
		CLUSTER_CONNECTOR_PROPERTIES :: #a_cluster_connector_properties{},
		PID :: pid(),
		REASON :: term().

start_link(CLUSTER_CONNECTOR_PROPERTIES) ->

	supervisor:start_link({local,?SERVER},?MODULE,[
		CLUSTER_CONNECTOR_PROPERTIES
	]).


%% ----------------------------
%% @doc
-spec init(ARGUMENTS) -> {ok, STATE} | {error, REASON}
	when
		ARGUMENTS :: list(),
		STATE :: term(),
		REASON :: term().

init([_CLUSTER_CONNECTOR_PROPERTIES]) ->

	HANDLER = #{
		id => a_cluster_connector_handler,
		start => {'a_cluster_connector_handler_gs','start_link',[
			#a_cluster_connector_handler_state{}
		]},
		restart => transient,
		shutdown => 5000,
		type => worker,
		modules => ['a_cluster_connector_handler_gs']
	},

	CHILD_SPECIFICATIONS = [HANDLER],
	case supervisor:check_childspecs(CHILD_SPECIFICATIONS) of
		ok ->
			MAX_RESTART = 1000,
			MAX_TIME_BETWEEN_RESTARTS = 3600,
			RESTART_STRATEGY = #{
				strategy => one_for_one,
				intensity => MAX_RESTART,
				period => MAX_TIME_BETWEEN_RESTARTS
			},
			{ok, {RESTART_STRATEGY,CHILD_SPECIFICATIONS}};
		{error,REASON} ->
			{error,REASON}
	end.
