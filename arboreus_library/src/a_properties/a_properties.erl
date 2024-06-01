%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 13. Feb 2024 17:58
%%%-------------------------------------------------------------------
-module(a_properties).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% Data types
-include("a_includes.hrl").

%% API
-export([
	test/0,
	get/2,
	get_all/1,
	put/2,
	delete/2
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Perform gen_server:call "get" for application properties process
-spec get(KEY,PROPERTIES_PID) -> {ok,VALUE} | {error,REASON}
	when
		KEY :: atom() | a_utf_text_string() | a_utf_text_binary(),
		PROPERTIES_PID :: pid(),
		VALUE :: any(),
		REASON :: term().

get(KEY,PROPERTIES_PID) ->

	gen_server:call(PROPERTIES_PID,{get,KEY}).


%% ----------------------------
%% @doc Perform gen_server:call "get" for application properties process
-spec get_all(PROPERTIES_PID) -> [{KEY,VALUE}]
	when
		PROPERTIES_PID :: pid(),
		KEY :: atom() | a_utf_text_string() | a_utf_text_binary(),
		VALUE :: any().

get_all(PROPERTIES_PID) ->

	gen_server:call(PROPERTIES_PID,get_all).


%% ----------------------------
%% @doc Perform gen_server:call "put" for application properties process
-spec put(KEY_VALUE_PAIR,PROPERTIES_PID) -> {ok,KEY_VALUE_PAIR} | {error,REASON}
	when
		KEY_VALUE_PAIR :: {KEY,VALUE},
		KEY :: atom() | a_utf_text_string() | a_utf_text_binary(),
		VALUE :: any(),
		PROPERTIES_PID :: pid(),
		REASON :: term().

put(KEY_VALUE_PAIR,PROPERTIES_PID) ->

	gen_server:call(PROPERTIES_PID,{put,KEY_VALUE_PAIR}).


%% ----------------------------
%% @doc Perform gen_server:call "delete" for application properties process
-spec delete(KEY,PROPERTIES_PID) -> {ok,deleted} | {error,REASON}
	when
		KEY :: atom() | a_utf_text_string() | a_utf_text_binary(),
		PROPERTIES_PID :: pid(),
		REASON :: term().

delete(KEY,PROPERTIES_PID) ->

	gen_server:call(PROPERTIES_PID,{delete,KEY}).
