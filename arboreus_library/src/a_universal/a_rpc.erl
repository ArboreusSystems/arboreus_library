%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2016, http://arboreus.system
%%% @doc Additional Remote Procedure Call services
%%%
%%% @end
%%% Created : 07. Окт. 2016 19:38
%%%-------------------------------------------------------------------
-module(a_rpc).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% API
-export([
	test/0,
	async_call/4,
	call/4
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Make asynchronous call on defined node
-spec call(NODE_NAME,MODULE,FUNCTION,ARGUMENTS) -> any()
	when
		NODE_NAME :: atom(),
		MODULE :: atom(),
		FUNCTION :: atom(),
		ARGUMENTS :: list().

call(NODE_NAME,MODULE,FUNCTION,ARGUMENTS) ->

	rpc:call(NODE_NAME,MODULE,FUNCTION,ARGUMENTS).


%% ----------------------------
%% @doc Make asynchronous call on defined node
-spec async_call(NODE_NAME,MODULE,FUNCTION,ARGUMENTS) -> any()
	when
		NODE_NAME :: atom(),
		MODULE :: atom(),
		FUNCTION :: atom(),
		ARGUMENTS :: list().

async_call(NODE_NAME,MODULE,FUNCTION,ARGUMENTS) ->

	rpc:yield(rpc:async_call(NODE_NAME,MODULE,FUNCTION,ARGUMENTS)).
