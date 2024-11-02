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
		#a_cluster_connector_handler_state{}
	]).


%% ----------------------------
%% @doc
-spec start_link(INIT_STATE) -> {'ok', PID} | 'ignore' | {'error', REASON}
	when
		INIT_STATE :: #a_cluster_connector_handler_state{},
		PID :: pid(),
		REASON :: term().

start_link(INIT_STATE) ->

	case supervisor:start_link({local,?SERVER},?MODULE,[INIT_STATE]) of
		{ok,SUPERVISOR_PID} ->

			{ok,_HANDLER_PID} = start_handler(SUPERVISOR_PID),

			{ok,SUPERVISOR_PID};

		START_LINK_REPLY ->
			START_LINK_REPLY
	end.


%% ----------------------------
%% @doc Init supervisor
-spec init(ARGUMENTS) -> {ok, STATE} | {error, REASON}
	when
		ARGUMENTS :: list(),
		STATE :: term(),
		REASON :: term().

init([_CLUSTER_CONNECTOR_PROPERTIES]) ->

	CHILD_SPECIFICATIONS = [],
	MAX_RESTART = 1000,
	MAX_TIME_BETWEEN_RESTARTS = 3600,
	SUPERVISOR_FLAGS = #{
		strategy => one_for_one,
		intensity => MAX_RESTART,
		period => MAX_TIME_BETWEEN_RESTARTS
	},
	{ok,{SUPERVISOR_FLAGS,CHILD_SPECIFICATIONS}}.


%% ----------------------------
%% @doc Start Cluster Connector handler process
-spec start_handler(SUPERVISOR_PID) -> {ok,HANDLER_PID} | {error,REASON}
	when
		SUPERVISOR_PID :: pid(),
		HANDLER_PID :: pid(),
		REASON :: term().

start_handler(SUPERVISOR_PID) ->

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

	SETUP_HANDLER = fun(IN_PID) -> {ok,IN_PID} end,

	case supervisor:start_child(SUPERVISOR_PID,HANDLER) of
		{ok,HANDLER_PID} -> SETUP_HANDLER(HANDLER_PID);
		{ok,HANDLER_PID,_INFO} -> SETUP_HANDLER(HANDLER_PID);
		{error,START_CHILD_ERROR} -> {error,START_CHILD_ERROR}
	end.
