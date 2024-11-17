%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 21. Jun 2024 12:37
%%%-------------------------------------------------------------------
-module(a_cluster_controller_db_gs).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").
-behaviour(gen_server).

%% Definitions
-define(SERVER,?MODULE).

%% State

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
		STATE :: #a_cluster_controller_db_state{},
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
		STATE :: #a_cluster_controller_db_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

init([STATE]) ->

	case a_cluster_controller_db:init() of
		{ok,TABLE_REFERENCE} ->
			process_flag(trap_exit,true),
			{ok,STATE#a_cluster_controller_db_state{
				ets_nodes = TABLE_REFERENCE
			}};
		{error,REASON} ->
			{error,REASON}
	end.


%% ----------------------------
%% @private
%% @doc Handling call messages
-spec handle_call(REQUEST,FROM,STATE) ->
	{reply,REPLY,NEW_STATE} | {reply,REPLY,NEW_STATE,TIMEOUT} |
	{noreply,NEW_STATE} | {noreply,NEW_STATE,TIMEOUT} |
	{stop,REASON,REPLY,NEW_STATE} | {stop,REASON,NEW_STATE}
	when
		REQUEST :: term(),
		FROM :: {PID, TAG},
		PID :: pid(),
		TAG :: term(),
		REPLY :: term(),
		STATE :: #a_cluster_controller_db_state{},
		NEW_STATE :: #a_cluster_controller_db_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

handle_call({delete_by_id,NODE_ID},_FROM,STATE) ->

	delete_by_id(NODE_ID,STATE);

handle_call({select_by_id,NODE_ID},_FROM,STATE) ->

	select_by_id(NODE_ID,STATE);

handle_call({is_added_by_id,NODE_ID},_FROM,STATE) ->

	is_added_by_id(NODE_ID,STATE);

handle_call({add_node,NODE_DATA},_FROM,STATE) ->

	add_node(NODE_DATA,STATE);

handle_call(get_all_nodes,_FROM,STATE) ->

	get_all_nodes(STATE);

handle_call({setup,HANDLER_PID,MONITOR_PID},_FROM,STATE) ->

	setup(HANDLER_PID,MONITOR_PID,STATE);

handle_call(REQUEST,FROM,STATE = #a_cluster_controller_db_state{}) ->

	ERROR = {error,undefined_call,self(),REQUEST,FROM,?MODULE,?FILE,?LINE},
	{reply,ERROR,STATE}.


%% ----------------------------
%% @private
%% @doc Handling cast messages
-spec handle_cast(REQUEST,STATE) ->
	{noreply,NEW_STATE} | {noreply,NEW_STATE,TIMEOUT} | {stop,REASON,NEW_STATE}
	when
		REQUEST :: term(),
		STATE :: #a_cluster_controller_db_state{},
		NEW_STATE :: #a_cluster_controller_db_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

handle_cast(_REQUEST,STATE) ->

	{noreply, STATE}.


%% ----------------------------
%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(INFO,STATE) ->
	{noreply,NEW_STATE} | {noreply,NEW_STATE,TIMEOUT} | {stop,REASON,NEW_STATE}
	when
		INFO :: timeout() | term(),
		STATE :: #a_cluster_controller_db_state{},
		NEW_STATE :: #a_cluster_controller_db_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

handle_info({'EXIT',_FROM,REASON},STATE) ->

	{stop,REASON,STATE};

handle_info(_INFO,STATE = #a_cluster_controller_db_state{}) ->

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
		STATE :: #a_cluster_controller_db_state{}.

terminate(_REASON,_STATE = #a_cluster_controller_db_state{}) ->

	ok.


%% ----------------------------
%% @private
%% @doc Convert process state when code is changed
-spec code_change(OLD_VERSION,STATE,EXTRA) -> {ok,NEW_STATE} | {error,REASON}
	when
		OLD_VERSION :: term() | {down, term()},
		STATE :: #a_cluster_controller_db_state{},
		NEW_STATE :: #a_cluster_controller_db_state{},
		EXTRA :: term(),
		REASON :: term().

code_change(_OLD_VERSION,STATE = #a_cluster_controller_db_state{},_EXTRA) ->

	{ok, STATE}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% ----------------------------
%% @doc Setup process
-spec setup(HANDLER_PID,MONITOR_PID,STATE) -> {reply,ok,STATE}
	when
		HANDLER_PID :: pid(),
		MONITOR_PID :: pid(),
		STATE :: #a_cluster_controller_db_state{}.

setup(HANDLER_PID,MONITOR_PID,STATE) ->

	{reply,ok,STATE#a_cluster_controller_db_state{
		handler = HANDLER_PID,
		monitor = MONITOR_PID
	}}.


%% ----------------------------
%% @doc Return all registered in ETS table nodes
-spec get_all_nodes(STATE) -> {reply,{ok,NODES},STATE}
	when
		STATE :: #a_cluster_controller_db_state{},
		NODES :: [#a_cluster_node_data{}].

get_all_nodes(STATE) ->

	NODES = ets:match_object(
		STATE#a_cluster_controller_db_state.ets_nodes,
		{'_','_','_','_','_','_'}
	),

	{reply,{ok,NODES},STATE}.


%% ----------------------------
%% @doc Add node to Cluster Controller
-spec add_node(NODE_DATA,STATE) -> {reply,OUTPUT,STATE}
	when
		OUTPUT :: {reply,OUTPUT_ADD_NODE,STATE},
		OUTPUT_ADD_NODE :: {ok,NODE_DATA} | {already_added,NODE_DATA} | {error,REASON},
		NODE_DATA :: #a_cluster_node_data{},
		STATE :: #a_cluster_controller_db_state{},
		REASON :: term().

add_node(NODE_DATA,STATE) when is_record(NODE_DATA,a_cluster_node_data) ->

	{reply,a_cluster_controller_db:add(
		NODE_DATA#a_cluster_node_data{id = a_node:node_id(
			a_node:name_string(
				NODE_DATA#a_cluster_node_data.name,
				NODE_DATA#a_cluster_node_data.server
			)
		)},
		STATE#a_cluster_controller_db_state.ets_nodes
	),STATE};

add_node(_NODE_DATA,STATE) ->

	{reply,{error,wrong_node_data},STATE}.


%% ----------------------------
%% @doc Check if node already added to the list
-spec is_added_by_id(NODE_ID,STATE) -> OUTPUT
	when
		NODE_ID :: a_id_32(),
		STATE :: #a_cluster_controller_db_state{},
		OUTPUT :: boolean().

is_added_by_id(NODE_ID,STATE) ->

	case a_cluster_controller_db:select_by_id(
		NODE_ID,STATE#a_cluster_controller_db_state.ets_nodes
	) of
		[] -> {reply,false,STATE};
		[_NODE_DATA] -> {reply,true,STATE}
	end.


%% ----------------------------
%% @doc Return selected by ID node data
-spec select_by_id(NODE_ID,STATE) -> OUTPUT
	when
		NODE_ID :: a_id_32(),
		STATE :: #a_cluster_controller_db_state{},
		OUTPUT :: [#a_cluster_node_data{}].

select_by_id(NODE_ID,STATE) ->

	{reply,a_cluster_controller_db:select_by_id(
		NODE_ID,STATE#a_cluster_controller_db_state.ets_nodes
	),STATE}.


%% ----------------------------
%% @doc Delete node by ID from list
-spec delete_by_id(NODE_ID,STATE) -> OUTPUT
	when
		NODE_ID :: a_id_32(),
		STATE :: #a_cluster_controller_db_state{},
		OUTPUT:: boolean().

delete_by_id(NODE_ID,STATE) ->

	{reply,a_cluster_controller_db:delete_by_id(
		NODE_ID,STATE#a_cluster_controller_db_state.ets_nodes
	),STATE}.