%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc A_structure_tree: Mnesia DB handler
%%%
%%% @end
%%% Created : 28. Апр. 2018 11:27
%%%-------------------------------------------------------------------
-module(astr_mdb).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Data types
-include("../include/types/types_general.hrl").
-include("../include/types/types_a_structure_tree.hrl").

%% Data models
-include("../include/records/records_a_structure_tree.hrl").

%% API
-export([
	test/0,
	init/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc
-spec init(Nodes) -> ok
	when
	Nodes :: [node()].

init(Nodes) ->
	create_schema(Nodes),
	create_data_model(Nodes),
	mnesia:info().


%% ----------------------------
%% @doc Create Mnesia DB schema
-spec create_schema(Nodes) -> ok | error
	when
	Nodes :: [node()].

create_schema(Nodes) ->
	a_mnesia:create_schema(Nodes).


%% ----------------------------
%% @doc Create data models

create_data_model(Nodes) ->
	{atomic,ok} = mnesia:create_table(astr_point,[
		{attributes,record_info(fields,astr_point)},
		{type,ordered_set},{index,[twig,kind]},
		{disc_copies,Nodes}
	]),
	{atomic,ok} = mnesia:create_table(astr_link,[
		{attributes,record_info(fields,astr_link)},
		{type,ordered_set},{index,[point_a,point_b]},
		{disc_copies,Nodes}
	]),
	{atomic,ok} = mnesia:create_table(astr_alias,[
		{attributes,record_info(fields,astr_alias)},
		{type,ordered_set},{index,[point]},
		{disc_copies,Nodes}
	]),
	{atomic,ok} = mnesia:create_table(astr_twig,[
		{attributes,record_info(fields,astr_twig)},
		{type,ordered_set},{disc_copies,Nodes}
	]),
	ok.