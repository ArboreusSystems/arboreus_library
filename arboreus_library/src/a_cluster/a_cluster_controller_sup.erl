%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 21. Jun 2024 12:32
%%%-------------------------------------------------------------------
-module(a_cluster_controller_sup).
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
%% @doc Start Cluster Controller with default properties
-spec start_link() -> {'ok',PID} | 'ignore' | {'error',REASON}
	when
		PID :: pid(),
		REASON :: term().

start_link() ->

	supervisor:start_link({local,?SERVER},?MODULE,[
		#a_cluster_controller_properties{}
	]).


%% ----------------------------
%% @doc Start Cluster Controller with defined properties
-spec start_link(CLUSTER_CONTROLLER_PROPERTIES) -> {'ok',PID} | 'ignore' | {'error',REASON}
	when
		CLUSTER_CONTROLLER_PROPERTIES :: #a_cluster_controller_properties{},
		PID :: pid(),
		REASON :: term().

start_link(CLUSTER_CONTROLLER_PROPERTIES) ->

	case supervisor:start_link({local,?SERVER},?MODULE,[
		CLUSTER_CONTROLLER_PROPERTIES
	]) of
		{ok,SUPERVISOR_PID} ->

			{ok,DB_PID} = start_db(SUPERVISOR_PID),
			{ok,HANDLER_PID} = start_handler(SUPERVISOR_PID),
			{ok,MONITOR_PID} = start_monitor(SUPERVISOR_PID),

			ok = setup(DB_PID,HANDLER_PID,MONITOR_PID),

			{ok,SUPERVISOR_PID};

		START_LINK_REPLY ->
			START_LINK_REPLY
	end.


%% ----------------------------
%% @doc Start Cluster Controller db process
-spec start_db(SUPERVISOR_PID) -> {ok,DB_PID} | {error,REASON}
	when
		SUPERVISOR_PID :: pid(),
		DB_PID :: pid(),
		REASON :: term().

start_db(SUPERVISOR_PID) ->

	DB = #{
		id => ?A_ID_CLUSTER_CONTROLLER_DB,
		start => {'a_cluster_controller_db_gs','start_link',[
			#a_cluster_controller_db_state{}
		]},
		restart => transient,
		shutdown => 5000,
		type => worker,
		modules => ['a_cluster_controller_db_gs']
	},

	SETUP_DB = fun(IN_PID) -> {ok,IN_PID} end,

	case supervisor:start_child(SUPERVISOR_PID,DB) of
		{ok,DB_PID} -> SETUP_DB(DB_PID);
		{ok,DB_PID,_INFO} -> SETUP_DB(DB_PID);
		{error,START_CHILD_ERROR} -> {error,START_CHILD_ERROR}
	end.


%% ----------------------------
%% @doc Start Cluster Controller handler process
-spec start_handler(SUPERVISOR_PID) -> {ok,HANDLER_PID} | {error,REASON}
	when
		SUPERVISOR_PID :: pid(),
		HANDLER_PID :: pid(),
		REASON :: term().

start_handler(SUPERVISOR_PID) ->

	HANDLER = #{
		id => ?A_ID_CLUSTER_CONTROLLER_HANDLER,
		start => {'a_cluster_controller_handler_gs','start_link',[
			#a_cluster_controller_handler_state{}
		]},
		restart => transient,
		shutdown => 5000,
		type => worker,
		modules => ['a_cluster_controller_handler_gs']
	},

	SETUP_HANDLER = fun(IN_PID) -> {ok,IN_PID} end,

	case supervisor:start_child(SUPERVISOR_PID,HANDLER) of
		{ok,HANDLER_PID} -> SETUP_HANDLER(HANDLER_PID);
		{ok,HANDLER_PID,_INFO} -> SETUP_HANDLER(HANDLER_PID);
		{error,START_CHILD_ERROR} -> {error,START_CHILD_ERROR}
	end.


%% ----------------------------
%% @doc Start Cluster Controller monitor process
-spec start_monitor(SUPERVISOR_PID) -> {ok,MONITOR_PID} | {error,REASON}
	when
		SUPERVISOR_PID :: pid(),
		MONITOR_PID :: pid(),
		REASON :: term().

start_monitor(SUPERVISOR_PID) ->

	MONITOR = #{
		id => ?A_ID_CLUSTER_CONTROLLER_MONITOR,
		start => {'a_cluster_controller_monitor_gs','start_link',[
			#a_cluster_controller_monitor_state{}
		]},
		restart => transient,
		shutdown => 5000,
		type => worker,
		modules => ['a_cluster_controller_monitor_gs']
	},

	SETUP_MONITOR = fun(IN_PID) -> {ok,IN_PID} end,

	case supervisor:start_child(SUPERVISOR_PID,MONITOR) of
		{ok, MONITOR_PID} -> SETUP_MONITOR(MONITOR_PID);
		{ok, MONITOR_PID,_INFO} -> SETUP_MONITOR(MONITOR_PID);
		{error,START_CHILD_ERROR} -> {error,START_CHILD_ERROR}
	end.


%% ----------------------------
%% @doc Setup Cluster Controller processes
-spec setup(DB_PID,HANDLER_PID,MONITOR_PID) -> ok
	when
		DB_PID :: pid(),
		HANDLER_PID :: pid(),
		MONITOR_PID :: pid().

setup(DB_PID,HANDLER_PID,MONITOR_PID) ->

	ok = gen_server:call(DB_PID,{setup,HANDLER_PID,MONITOR_PID}),
	ok = gen_server:call(HANDLER_PID,{setup,DB_PID,MONITOR_PID}),
	ok = gen_server:call(MONITOR_PID,{setup,DB_PID,HANDLER_PID}),

	ok.


%% ----------------------------
%% @doc Init supervisor
-spec init(ARGUMENTS) -> {ok,STATE} | {error,REASON}
	when
		ARGUMENTS :: list(),
		STATE :: term(),
		REASON :: term().

init([_CLUSTER_CONTROLLER_PROPERTIES]) ->

	CHILD_SPECIFICATIONS = [],
	MAX_RESTART = 1000,
	MAX_TIME_BETWEEN_RESTARTS = 3600,
	SUPERVISOR_FLAGS = #{
		strategy => one_for_one,
		intensity => MAX_RESTART,
		period => MAX_TIME_BETWEEN_RESTARTS
	},
	{ok,{SUPERVISOR_FLAGS,CHILD_SPECIFICATIONS}}.

