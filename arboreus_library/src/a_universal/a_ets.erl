%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 25. Oct 2024 11:30
%%%-------------------------------------------------------------------
-module(a_ets).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% System includes

%% Application includes
-include("a_includes.hrl").

%% API
-export([

	test/0,

	table_existed/1

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Checking ETS table existence.
-spec table_existed(TABLE) -> boolean()
	when TABLE :: atom().

table_existed(TABLE) -> lists:member(TABLE,ets:all()).