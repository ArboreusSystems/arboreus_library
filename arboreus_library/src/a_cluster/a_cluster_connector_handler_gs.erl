%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 21. Jun 2024 12:31
%%%-------------------------------------------------------------------
-module(a_cluster_connector_handler_gs).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").
-behaviour(gen_server).

%% Definitions
-define(SERVER, ?MODULE).

%% State

%% System includes

%% Application includes
-include("../../include/a_includes.hrl").

%% API
-export([

	test/0,

	start_link/1,
	init/1,
	handle_call/3, handle_cast/2, handle_info/2,
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
		STATE :: #a_cluster_connector_handler_state{},
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
		STATE :: #a_cluster_connector_handler_state{},
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
		FROM :: {PID,TAG}, PID :: pid(), TAG :: term(),
		REPLY :: term(),
		STATE :: #a_cluster_connector_handler_state{},
		NEW_STATE :: #a_cluster_connector_handler_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

handle_call({set_node_data,NODE_DATA},_FROM,STATE) -> set_node_data(NODE_DATA,STATE);

handle_call(node_data,_FROM,STATE) -> node_data(STATE);

handle_call(delete_node,_FROM,STATE) -> delete_node(STATE);

handle_call(add_node,_FROM,STATE) -> add_node(STATE);

handle_call({get_nodes_by_type,TYPE},_FROM,STATE) -> get_nodes_by_type(TYPE,STATE);

handle_call(REQUEST,FROM,STATE = #a_cluster_connector_handler_state{}) ->

	ERROR = {error,undefined_call,self(),REQUEST,FROM,?MODULE,?FILE,?LINE},
	{reply,ERROR,STATE}.


%% ----------------------------
%% @private
%% @doc Handling cast messages
-spec handle_cast(REQUEST, STATE) ->
	{noreply,NEW_STATE} | {noreply,NEW_STATE,TIMEOUT} | {stop,REASON,NEW_STATE}
	when
		REQUEST :: term(),
		STATE :: #a_cluster_connector_handler_state{},
		NEW_STATE :: #a_cluster_connector_handler_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

handle_cast(_REQUEST,STATE = #a_cluster_connector_handler_state{}) ->

	{noreply, STATE}.


%% ----------------------------
%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(INFO,STATE) ->
	{noreply,NEW_STATE} | {noreply,NEW_STATE,TIMEOUT} | {stop,REASON,NEW_STATE}
	when
		INFO :: timeout() | term(),
		STATE :: #a_cluster_connector_handler_state{},
		NEW_STATE :: #a_cluster_connector_handler_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

handle_info({'EXIT',_FROM,REASON},STATE) ->

	{stop,REASON,STATE};

handle_info(_INFO, STATE = #a_cluster_connector_handler_state{}) ->

	{noreply, STATE}.


%% ----------------------------
%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(REASON,STATE) -> term()
	when
		REASON :: normal | shutdown | {shutdown, term()} | term(),
		STATE :: #a_cluster_connector_handler_state{}.

terminate(_REASON, _STATE = #a_cluster_connector_handler_state{}) ->

	ok.


%% ----------------------------
%% @private
%% @doc Convert process state when code is changed
-spec code_change(OLD_VERSION,STATE,EXTRA) -> {ok,NEW_STATE} | {error,REASON}
	when
		OLD_VERSION :: term() | {down, term()},
		STATE :: #a_cluster_connector_handler_state{},
		NEW_STATE :: #a_cluster_connector_handler_state{},
		EXTRA :: term(),
		REASON :: term().

code_change(_OLD_VERSION,STATE = #a_cluster_connector_handler_state{},_EXTRA) ->

	{ok, STATE}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% ----------------------------
%% @doc Add current node to Cluster Controller
-spec add_node(STATE) -> {reply,ADD_NODE_RESULT,STATE}
	when
		STATE :: #a_cluster_connector_handler_state{},
		ADD_NODE_RESULT :: {ok,NODE_DATA} | {already_added,NODE_DATA} | {error,REASON},
		NODE_DATA :: #a_cluster_node_data{},
		REASON :: term().

add_node(STATE) ->

	ADD_NODE_HANDLER = STATE#a_cluster_connector_handler_state.handler_add_node,
	{reply,ADD_NODE_HANDLER(
		STATE#a_cluster_connector_handler_state.data,
		STATE#a_cluster_connector_handler_state.main_controller
	),STATE}.


%% ----------------------------
%% @doc Return node name by type from Cluster Controller
-spec get_nodes_by_type(TYPE,STATE) -> {reply,GET_NODE_BY_TYPE_RESULT,STATE}
	when
		TYPE :: any(),
		STATE :: #a_cluster_connector_handler_state{},
		GET_NODE_BY_TYPE_RESULT :: [a_node_name_atom()] | {error,REASON},
		REASON :: term().

get_nodes_by_type(TYPE,STATE) ->

	GET_NODE_BY_TYPE_HANDLER = STATE#a_cluster_connector_handler_state.handler_get_nodes_by_type,
	{reply,GET_NODE_BY_TYPE_HANDLER(
		TYPE,
		STATE#a_cluster_connector_handler_state.main_controller
	),STATE}.


%% ----------------------------
%% @doc Delete node from the list on Cluster Controller
-spec delete_node(STATE) -> {reply,DELETE_NODE_RESULT,STATE}
	when
		STATE :: #a_cluster_connector_handler_state{},
		DELETE_NODE_RESULT :: boolean().

delete_node(STATE) ->

	DELETE_NODE_HANDLER = STATE#a_cluster_connector_handler_state.handler_delete_node_by_id,
	NODE_DATA = STATE#a_cluster_connector_handler_state.data,
	{reply,DELETE_NODE_HANDLER(
		NODE_DATA#a_cluster_node_data.id,
		STATE#a_cluster_connector_handler_state.main_controller
	),STATE}.


%% ----------------------------
%% @doc Return node data
-spec node_data(STATE) -> {reply,{ok,DATA},STATE}
	when
		STATE :: #a_cluster_connector_handler_state{},
		DATA :: #a_cluster_node_data{}.

node_data(STATE) ->

	{reply,{ok,STATE#a_cluster_connector_handler_state.data},STATE}.


%% ----------------------------
%% @doc Set node data for handler
-spec set_node_data(NODE_DATA,STATE) -> {reply,ok,STATE}
	when
		NODE_DATA :: #a_cluster_node_data{},
		STATE :: #a_cluster_connector_handler_state{}.

set_node_data(NODE_DATA,STATE) ->

	{reply,ok,STATE#a_cluster_connector_handler_state{
		data = NODE_DATA
	}}.