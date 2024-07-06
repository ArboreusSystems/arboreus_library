%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2024 13:19
%%%-------------------------------------------------------------------
-module(a_network).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% System includes

%% Application includes
-include("a_includes.hrl").

%% API
-export([

	test/0,

	start/3,
	start_epmd/1

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Start network for Erlang node
-spec start(TIMEOUT,KERNEL_PROPERTIES,COOKIE) -> ok | {error,REASON}
	when
		TIMEOUT :: a_time_unix_milliseconds(),
		KERNEL_PROPERTIES :: list(),
		COOKIE :: list() | atom(),
		REASON :: term().

start(TIMEOUT,KERNEL_PROPERTIES,COOKIE) when is_list(COOKIE) ->

	start(TIMEOUT,KERNEL_PROPERTIES,list_to_atom(COOKIE));

start(TIMEOUT,KERNEL_PROPERTIES,COOKIE) when is_atom(COOKIE) ->

	ok = start_epmd(TIMEOUT),

	case net_kernel:start(KERNEL_PROPERTIES) of
		{error,REASON} ->
			{error,{net_kernel,REASON}};
		_ ->
			case erlang:set_cookie(COOKIE) of
				true ->
					ok;
				_ ->
					net_kernel:stop(),
					{error,cookie}
			end
	end.


%% ----------------------------
%% @doc Start epmd daemon
-spec start_epmd(TIMEOUT) -> ok
	when TIMEOUT :: a_time_unix_milliseconds().

start_epmd(TIMEOUT) ->

	os:cmd("epmd &"),
	timer:sleep(TIMEOUT),
	ok.