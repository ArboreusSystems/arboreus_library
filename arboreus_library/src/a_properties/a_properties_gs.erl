%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 11. Feb 2024 16:58
%%%-------------------------------------------------------------------
-module(a_properties_gs).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").
-behaviour(gen_server).

%% Data types
-include("a_includes.hrl").

%% API
-export([
	test/0,
	start_link/1,
	init/1,
	handle_call/3,handle_cast/2,handle_info/2,
	terminate/2,
	code_change/3
]).

%% Definitions
-define(SERVER,?MODULE).
-define(A_PROPERTIES_ID_STORAGE,a_properties_storage).


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
		STATE :: #a_properties_state{},
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
		STATE :: #a_properties_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

init([STATE]) ->

	case ets:new(?A_PROPERTIES_ID_STORAGE,[
		ordered_set,private,{keypos,#a_properties_pair.key}
	]) of
		TABLE_REFERENCE when is_reference(TABLE_REFERENCE) ->
			on_init(STATE),
			process_flag(trap_exit,true),
			{ok,STATE#a_properties_state{
				storage_id = ?A_PROPERTIES_ID_STORAGE,
				storage_tid = TABLE_REFERENCE
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
		FROM :: {PID,TAG},PID :: pid(),TAG :: term(),
		REPLY :: term(),
		STATE :: #a_properties_state{},
		NEW_STATE :: #a_properties_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

handle_call({delete,KEY},_FROM,STATE = #a_properties_state{}) ->

	{reply,delete(KEY,STATE),STATE};

handle_call(get_all,_FROM,STATE = #a_properties_state{}) ->

	{reply,get_all(STATE),STATE};

handle_call({get,KEY},_FROM,STATE = #a_properties_state{}) ->

	{reply,get(KEY,STATE),STATE};

handle_call({put,{KEY,VALUE}},_FROM,STATE = #a_properties_state{}) ->

	{reply,put(KEY,VALUE,STATE),STATE};

handle_call(REQUEST,FROM,STATE = #a_properties_state{}) ->

	ERROR = {error,undefined_call,self(),REQUEST,FROM,?MODULE,?FILE,?LINE},
	{reply,ERROR,STATE}.


%% ----------------------------
%% @private
%% @doc Handling cast messages
-spec handle_cast(REQUEST,STATE) ->
	{noreply,NEW_STATE} |    {noreply,NEW_STATE,TIMEOUT} | {stop,REASON,NEW_STATE}
	when
		REQUEST :: term(),
		STATE :: #a_properties_state{},
		NEW_STATE :: #a_properties_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

handle_cast(_REQUEST,STATE = #a_properties_state{}) ->

	{noreply,STATE}.


%% ----------------------------
%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(INFO,STATE) ->
	{noreply,NEW_STATE} | {noreply,NEW_STATE,TIMEOUT} | {stop,REASON,NEW_STATE}
	when
		INFO :: timeout() | term(),
		STATE :: #a_properties_state{},
		NEW_STATE :: #a_properties_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

handle_info({'EXIT',_FROM,REASON},STATE) ->

	{stop,REASON,STATE};

handle_info(_INFO,STATE = #a_properties_state{}) ->

	{noreply,STATE}.


%% ----------------------------
%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(REASON,STATE) -> term()
	when
		REASON :: normal | shutdown | {shutdown,term()} | term(),
		STATE :: #a_properties_state{}.

terminate(_REASON,_STATE = #a_properties_state{}) ->

	ok.


%% ----------------------------
%% @private
%% @doc Convert process state when code is changed
-spec code_change(OLD_VERSION,STATE,EXTRA) -> {ok,NEW_STATE} | {error,REASON}
	when
		OLD_VERSION :: term() | {down,term()},
		STATE :: #a_properties_state{},
		NEW_STATE :: #a_properties_state{},
		EXTRA :: term(),
		REASON :: term().

code_change(_OLD_VERSION,STATE = #a_properties_state{},_EXTRA) ->

	{ok,STATE}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% ----------------------------
%% @doc Perform on-init function
-spec on_init(STATE) -> ok
	when STATE :: #a_properties_state{}.

on_init(STATE) when STATE#a_properties_state.on_init == true ->

	erlang:apply(
		STATE#a_properties_state.on_init_module,
		STATE#a_properties_state.on_init_function,
		STATE#a_properties_state.on_init_parameters
	);

on_init(_STATE) -> ok.

%% ----------------------------
%% @doc Put key-value pair to proplist storage
-spec put(KEY,VALUE,STATE) -> {ok,{KEY,VALUE}} | {error,REASON}
	when
		KEY :: atom() | a_utf_text_string() | a_utf_text_binary(),
		VALUE :: any(),
		STATE :: #a_properties_state{},
		REASON :: term().

put(KEY,VALUE,STATE) ->

	case ets:insert(
		STATE#a_properties_state.storage_tid,
		#a_properties_pair{key = KEY,value = VALUE}
	) of
		true -> {ok,{KEY,VALUE}};
		_ -> {error,not_inserted}
	end.


%% ----------------------------
%% @doc Return value by key from process state storage
-spec get(KEY,STATE) -> {ok,VALUE} | {error,REASON}
	when
		KEY :: atom() | a_utf_text_string() | a_utf_text_binary(),
		STATE :: #a_properties_state{},
		VALUE :: any(),
		REASON :: term().

get(KEY,STATE) ->

	case ets:match_object(
		STATE#a_properties_state.storage_tid,
		{'_',KEY,'_'}
	) of
		[VALUE] when is_record(VALUE,a_properties_pair) ->
			{ok,VALUE};
		_ ->
			{error,no_key}
	end.


%% ----------------------------
%% @doc Return all properties from storage
-spec get_all(STATE) -> [{KEY,VALUE}]
	when
		STATE :: #a_properties_state{},
		KEY :: atom() | a_utf_text_string() | a_utf_text_binary(),
		VALUE :: any().

get_all(STATE) ->

	ets:match_object(
		STATE#a_properties_state.storage_tid,
		{'_','_','_'}
	).


%% ----------------------------
%% @doc Delete key-value pair from storage
-spec delete(KEY,STATE) -> {ok,deleted} | {error,REASON}
	when
		KEY :: atom() | a_utf_text_string() | a_utf_text_binary(),
		STATE :: #a_properties_state{},
		REASON :: term().

delete(KEY,STATE) ->

	case get(KEY,STATE) of
		{ok,KEY_VALUE_PAIR} ->
			case ets:delete_object(
				STATE#a_properties_state.storage_tid,
				KEY_VALUE_PAIR
			) of
				true -> {ok,deleted};
				_ -> {error,undefined_error}
			end;
		ERROR_GET ->
			ERROR_GET
	end.