%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 21. Jun 2024 12:33
%%%-------------------------------------------------------------------
-module(a_cluster_controller_handler_gs).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").
-behaviour(gen_server).

%% Definitions
-define(SERVER, ?MODULE).

%% System includes

%% Application includes
-include("../../include/a_includes.hrl").

%% API
-export([

	test/0,

	start_link/1,
	init/1,
	handle_call/3,handle_cast/2,handle_info/2,
	terminate/2,
	code_change/3

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%%%===================================================================
%%% API
%%%===================================================================
%% ----------------------------
%% @doc Spawns the server and registers the local name (unique)
-spec start_link(STATE) -> {ok,PID} | ignore | {error,REASON}
	when
		STATE :: #a_cluster_controller_handler_state{},
		PID :: pid(),
		REASON :: term().

start_link(STATE) ->

	gen_server:start_link(?MODULE,[STATE],[]).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% ----------------------------
%% @private
%% @doc Initializes the server
-spec init(ARGUMENTS) ->
	{ok,STATE} | {ok,STATE,TIMEOUT} | {stop,REASON} | {error,REASON} | ignore
	when
		ARGUMENTS :: term(),
		STATE :: #a_cluster_controller_handler_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

init([STATE]) ->

	process_flag(trap_exit, true),
	{ok,STATE}.


%% ----------------------------
%% @private
%% @doc Handling call messages
-spec handle_call(REQUEST,FROM,STATE) ->
	{reply,REPLY,NEW_STATE} | {reply,REPLY,NEW_STATE,TIMEOUT} |
	{noreply,NEW_STATE} | {noreply,NEW_STATE,TIMEOUT} |
	{stop,REASON,REPLY,NEW_STATE} | {stop,REASON,NEW_STATE}
	when
		REQUEST :: term(),
		FROM :: {PID,TAG},
		PID :: pid(),
		TAG :: term(),
		REPLY :: term(),
		STATE :: #a_cluster_controller_handler_state{},
		NEW_STATE :: #a_cluster_controller_handler_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

handle_call({define_get_nodes_handler,HANDLER},_FROM,STATE)
	when is_function(HANDLER) ->

	define_get_nodes_handler(HANDLER,STATE);

handle_call({get_nodes_by_type,TYPE},_FROM,STATE) ->

	get_nodes_by_type(TYPE,STATE);

handle_call({get_nodes_by_handler,TYPE},_FROM,STATE) ->

	get_nodes_by_handler(TYPE,STATE);

handle_call(node_name,_FROM,STATE) ->

	node_name(STATE);

handle_call({setup,DB_PID,MONITOR_PID},_FROM,STATE) ->

	setup(DB_PID,MONITOR_PID,STATE);

handle_call(REQUEST,FROM,STATE = #a_cluster_controller_handler_state{}) ->

	ERROR = {error,undefined_call,self(),REQUEST,FROM,?MODULE,?FILE,?LINE},
	{reply,ERROR,STATE}.


%% ----------------------------
%% @private
%% @doc Handling cast messages
-spec handle_cast(REQUEST,STATE) ->
	{noreply,NEW_STATE} | {noreply,NEW_STATE,TIMEOUT} | {stop,REASON,NEW_STATE}
	when
		REQUEST :: term(),
		STATE :: #a_cluster_controller_handler_state{},
		NEW_STATE :: #a_cluster_controller_handler_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

handle_cast(_REQUEST,STATE = #a_cluster_controller_handler_state{}) ->

	{noreply,STATE}.


%% ----------------------------
%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(INFO,STATE) ->
	{noreply,NEW_STATE} | {noreply,NEW_STATE,TIMEOUT} | {stop,REASON,NEW_STATE}
	when
		INFO :: timeout() | term(),
		STATE :: #a_cluster_controller_handler_state{},
		NEW_STATE :: #a_cluster_controller_handler_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

handle_info({'EXIT',_FROM,REASON},STATE) ->

	{stop,REASON,STATE};

handle_info(_INFO,STATE = #a_cluster_controller_handler_state{}) ->

	{noreply,STATE}.


%% ----------------------------
%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(REASON,STATE) -> term()
	when
		REASON :: normal | shutdown | {shutdown, term()} | term(),
		STATE :: #a_cluster_controller_handler_state{}.

terminate(_REASON,_STATE = #a_cluster_controller_handler_state{}) ->

	ok.


%% ----------------------------
%% @private
%% @doc Convert process state when code is changed
-spec code_change(OLD_VERSION,STATE,EXTRA) -> {ok,NEW_STATE} | {error,REASON}
	when
		OLD_VERSION :: term() | {down, term()},
		STATE :: #a_cluster_controller_handler_state{},
		NEW_STATE :: #a_cluster_controller_handler_state{},
		EXTRA :: term(),
		REASON :: term().

code_change(_OLD_VERSION,STATE = #a_cluster_controller_handler_state{},_EXTRA) ->

	{ok, STATE}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% ----------------------------
%% @doc Setup process
-spec setup(DB_PID,MONITOR_PID,STATE) -> {reply,ok,STATE}
	when
		DB_PID :: pid(),
		MONITOR_PID :: pid(),
		STATE :: #a_cluster_controller_handler_state{}.

setup(DB_PID,MONITOR_PID,STATE) ->

	{reply,ok,STATE#a_cluster_controller_handler_state{
		db = DB_PID,
		monitor = MONITOR_PID
	}}.


%% ----------------------------
%% @doc Return Cluster Controller node name.
-spec node_name(STATE) -> {reply,{ok,NODE_NAME},STATE}
	when
		STATE :: #a_cluster_controller_handler_state{},
		NODE_NAME :: a_node_name_atom().

node_name(STATE) -> {reply,{ok,node()},STATE}.


%% ----------------------------
%% @doc Return list of nodes filtered by type
-spec get_nodes_by_type(TYPE,STATE) -> {reply,NODES,STATE}
	when
		TYPE :: any(),
		STATE :: #a_cluster_controller_handler_state{},
		NODES :: [#a_cluster_node_data{}].

get_nodes_by_type(TYPE,STATE) ->

	{ok,ALL_NODES} = gen_server:call(
		STATE#a_cluster_controller_handler_state.db,
		get_all_nodes
	),

	{reply,lists:filter(
		fun(NODE_DATA) -> NODE_DATA#a_cluster_node_data.type =:= TYPE end,
		ALL_NODES
	),STATE}.


%% ----------------------------
%% @doc Return node data
-spec get_nodes_by_handler(TYPE,STATE) -> {reply,{ok,NODES_DATA},STATE} | {reply,{error,REASON},STATE}
	when
		TYPE :: any(),
		STATE :: #a_cluster_controller_handler_state{},
		NODES_DATA :: [#a_cluster_node_data{}],
		REASON :: term().

get_nodes_by_handler(PROPERTIES,STATE) ->

	{ok,ALL_NODES} = gen_server:call(
		STATE#a_cluster_controller_handler_state.db,
		get_all_nodes
	),

	GET_NODES_BY_HANDLER = STATE#a_cluster_controller_handler_state.get_nodes_by_handler,
	case GET_NODES_BY_HANDLER(PROPERTIES,ALL_NODES) of
		{error,REASON} -> {reply,{error,REASON},STATE};
		no_node -> {reply,{error,no_node},STATE};
		no_type -> {reply,{error,no_type},STATE};
		NODES_DATA -> {reply,{ok,NODES_DATA},STATE}
	end.


%% ----------------------------
%% @doc Define handler for getting node
-spec define_get_nodes_handler(HANDLER,STATE) -> {reply,ok,STATE}
	when
		HANDLER :: fun(),
		STATE :: #a_cluster_controller_handler_state{}.

define_get_nodes_handler(HANDLER,STATE) when is_function(HANDLER) ->

	{reply,ok,STATE#a_cluster_controller_handler_state{
		get_nodes_by_handler = HANDLER
	}};

define_get_nodes_handler(_HANDLER,STATE) ->

	{reply,{error,handler_not_function},STATE}.
