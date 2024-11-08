%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 30. Oct 2024 20:58
%%%-------------------------------------------------------------------
-module(a_cluster_controller_default).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% System includes

%% Application includes
-include("../../include/a_includes.hrl").

%% API
-export([

	test/0,

	handler_state/0

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Return default handler state
-spec handler_state() -> STATE
	when STATE :: #a_cluster_controller_handler_state{}.

handler_state() ->

	#a_cluster_controller_handler_state{}.



