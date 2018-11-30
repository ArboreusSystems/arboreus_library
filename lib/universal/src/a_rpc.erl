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
-spec call(Node_name,Module,Function,Arguments) -> any()
	when
	Node_name :: atom(),
	Module :: atom(),
	Function :: atom(),
	Arguments :: list().

call(Node_name,Module,Function,Arguments) ->
	rpc:call(Node_name,Module,Function,Arguments).


%% ----------------------------
%% @doc Make asynchronous call on defined node
-spec async_call(Node_name,Module,Function,Arguments) -> any()
	when
	Node_name :: atom(),
	Module :: atom(),
	Function :: atom(),
	Arguments :: list().

async_call(Node_name,Module,Function,Arguments) ->
	rpc:yield(rpc:async_call(Node_name,Module,Function,Arguments)).