%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Areboreus users Mnesia schema handler
%%%
%%% @end
%%% Created : 06/03/2018 at 13:26
%%%-------------------------------------------------------------------
-module(ause_mdb).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Data Constants

%% Data types
-include("../data_models/types/types_general.hrl").
-include("../data_models/types/types_time.hrl").
-include("../data_models/types/types_a_users.hrl").

%% Data models
-include("../data_models/records/records_a_users.hrl").

%% API
-export([
	test/0,
	init/0,
	create_model/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Initialize the data schema in Mnesia DB

init() ->
	ok = a_mnesia:create_schema([node()]),
	ok = create_model([node()]),
	ok.


%% ----------------------------
%% @doc Create a_users data model in Mnesia DB
-spec create_model(Nodes) -> ok
	when
	Nodes :: [node()].

create_model(Nodes) ->
	{atomic,ok} = mnesia:create_table(a_user,[
		{attributes,record_info(fields,a_user)},
		{type,ordered_set},{disc_copies,Nodes}
	]),
	{atomic,ok} = mnesia:create_table(ause_login,[
		{attributes,record_info(fields,ause_login)},
		{type,ordered_set},{index,[kind,user]},
		{disc_copies,Nodes}
	]),
	{atomic,ok} = mnesia:create_table(ause_login_kind,[
		{attributes,record_info(fields,ause_login_kind)},
		{type,ordered_set},{disc_copies,Nodes}
	]),
	{atomic,ok} = mnesia:create_table(a_user_properties,[
		{attributes,record_info(fields,a_user_properties)},
		{type,ordered_set},{index,[first_name,last_name]},
		{disc_copies,Nodes}
	]),
	ok.