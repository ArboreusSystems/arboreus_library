%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus user handler application
%%%
%%% @end
%%% Created : 06/03/2018 at 11:08
%%%-------------------------------------------------------------------
-module(a_users).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").
-behaviour(application).

%% Constants

%% Data types

%% Data models

%% Application callbacks
-export([
	test/0,
	start/2,
	start_phase/3,
	stop/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Application start phases handler
-spec start_phase(Phase,Type,Arguments) -> any()
	when
	Phase :: term(),
	Type :: normal,
	Arguments :: list().

start_phase(Phase,Type,Arguments) ->
	io:format("start_phase(~p,~p,~p).~n",[Phase,Type,Arguments]).


%% ----------------------------
%% @doc Application start
-spec start(Start_type,Start_arguments) ->
	{ok, pid()} | {ok, pid(), State :: term()} | {error, Reason :: term()}
	when
	Start_type :: normal | {takeover, node()} | {failover, node()},
	Start_arguments :: term().

start(_Start_type,_Start_arguments) ->
	a_users_supervisor:start_link().


%% ----------------------------
%% @doc Application stop	
-spec stop(State :: term()) -> term().

stop(_State) -> ok.
