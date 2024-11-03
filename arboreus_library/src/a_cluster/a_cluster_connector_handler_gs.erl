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
-spec start_link(STATE) -> {ok, PID} | ignore | {error, REASON}
	when
		STATE :: #a_cluster_connector_handler_state{},
		PID :: pid(),
		REASON :: term().

start_link(STATE) ->

	gen_server:start_link(?MODULE, [STATE], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% ----------------------------
%% @private
%% @doc Initializes the server
-spec init(ARGUMENTS) ->
	{ok, STATE} | {ok, STATE, TIMEOUT} | {stop, REASON} | {error, REASON} | ignore
	when
		ARGUMENTS :: term(),
		STATE :: #a_cluster_connector_handler_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

init([_STATE]) ->

	process_flag(trap_exit, true),
	{ok, #a_cluster_connector_handler_state{}}.


%% ----------------------------
%% @private
%% @doc Handling call messages
-spec handle_call(REQUEST, FROM, STATE) ->
	{reply, REPLY, NEW_STATE} | {reply, REPLY, NEW_STATE, TIMEOUT} |
	{noreply, NEW_STATE} | {noreply, NEW_STATE, TIMEOUT} |
	{stop, REASON, REPLY, NEW_STATE} | {stop, REASON, NEW_STATE}
	when
		REQUEST :: term(),
		FROM :: {PID, TAG}, PID :: pid(), TAG :: term(),
		REPLY :: term(),
		STATE :: #a_cluster_connector_handler_state{},
		NEW_STATE :: #a_cluster_connector_handler_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

handle_call({add_node,NODE_DATA},_FROM,STATE) ->

	add_node(NODE_DATA,STATE);

handle_call(REQUEST,FROM,STATE = #a_cluster_connector_handler_state{}) ->

	ERROR = {error,undefined_call,self(),REQUEST,FROM,?MODULE,?FILE,?LINE},
	{reply,ERROR,STATE}.


%% ----------------------------
%% @private
%% @doc Handling cast messages
-spec handle_cast(REQUEST, STATE) ->
	{noreply, NEW_STATE} | {noreply, NEW_STATE, TIMEOUT} | {stop, REASON, NEW_STATE}
	when
		REQUEST :: term(),
		STATE :: #a_cluster_connector_handler_state{},
		NEW_STATE :: #a_cluster_connector_handler_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

handle_cast(_REQUEST, STATE = #a_cluster_connector_handler_state{}) ->

	{noreply, STATE}.


%% ----------------------------
%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(INFO, STATE) ->
	{noreply, NEW_STATE} | {noreply, NEW_STATE, TIMEOUT} | {stop, REASON, NEW_STATE}
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
-spec terminate(REASON, STATE) -> term()
	when
		REASON :: normal | shutdown | {shutdown, term()} | term(),
		STATE :: #a_cluster_connector_handler_state{}.

terminate(_REASON, _STATE = #a_cluster_connector_handler_state{}) ->

	ok.


%% ----------------------------
%% @private
%% @doc Convert process state when code is changed
-spec code_change(OLD_VERSION, STATE, EXTRA) -> {ok, NEW_STATE} | {error, REASON}
	when
		OLD_VERSION :: term() | {down, term()},
		STATE :: #a_cluster_connector_handler_state{},
		NEW_STATE :: #a_cluster_connector_handler_state{},
		EXTRA :: term(),
		REASON :: term().

code_change(_OLD_VERSION, STATE = #a_cluster_connector_handler_state{}, _EXTRA) ->

	{ok, STATE}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% ----------------------------
%% @doc

add_node(_NODE_DATA,STATE) ->

	{reply,ok,STATE}.