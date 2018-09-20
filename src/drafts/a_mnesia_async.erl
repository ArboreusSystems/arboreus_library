%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2016, http://arboreus.system
%%% @doc Remote async mnesia functionality call
%%%
%%% @end
%%% Created : 07. Окт. 2016 19:55
%%%-------------------------------------------------------------------
-module(a_mnesia_async).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% API
-export([
	test/0,
	activate_checkpoint/2,
	activity/5,
	add_table_index/3,
	all_keys/2,
	async_dirty/3,
	backup/2,
	backup_checkpoint/3,
	change_config/3,
	change_table_access_mode/3,
	change_table_load_order/3
]).

%% Module Include Start
-include("../Handler/a.hrl").
%% Module Include End


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Change table load priority
-spec change_table_load_order(Node_name,Table,Load_order) -> {aborted,_Reason} | {atomic, ok}
	when
		Node_name :: node(),
		Table :: table_name(),
		Load_order :: number().

change_table_load_order(Node_name,Table,Load_order) ->
	a_rpc:async_call(
		Node_name,mnesia,change_table_load_order,
		[Table,Load_order]
	).


%% ----------------------------
%% @doc Change table acces mode
-spec change_table_access_mode(Node_name,Table,Access_mode) -> {aborted,_Reason} | {atomic, ok}
	when
		Node_name :: node(),
		Table :: table_name(),
		Access_mode :: read_write | read_only.

change_table_access_mode(Node_name,Table,Access_mode) ->
	a_rpc:async_call(
		Node_name,mnesia,change_table_access_mode,
		[Table,Access_mode]
	).


%% ----------------------------
%% @doc Change mnesia configuration
-spec change_config(Node_name,Config,Value) -> {error,_Reason} | {ok,_Return_value}
	when
		Node_name :: node(),
		Config :: extra_db_nodes | dc_dump_limit,
		Value :: number().

change_config(Node_name,Config,Value) ->
	a_rpc:async_call(
		Node_name,mnesia,change_config,
		[Config,Value]
	).


%% ----------------------------
%% @doc The tables are backed up to external media using backup module BackupMod.
-spec backup_checkpoint(Node_name,Name,File) -> ok | {error,_Reason}
	when
		Node_name :: node(),
		Name :: atom(),
		File :: file_path_string().

backup_checkpoint(Node_name,Name,File) ->
	a_rpc:async_call(
		Node_name,mnesia,backup_checkpoint,
		[Name,File]
	).


%% ----------------------------
%% @doc Activates a new checkpoint covering all Mnesia tables
-spec backup(Node_name,File) -> ok | {error,_Reason}
	when
		Node_name :: node(),
		File :: file_path_string().

backup(Node_name,File) ->
	a_rpc:async_call(
		Node_name,mnesia,backup,
		[File]
	).


%% ----------------------------
%% @doc Calls the Function in a context that is not protected by a transaction
-spec async_dirty(Node_name,Function,Arguments) -> result_of_function() | {_Reason,_Stack}
	when
		Node_name :: node(),
		Function :: function(),
		Arguments :: list().

async_dirty(Node_name,Function,Arguments) ->
	a_rpc:async_call(
		Node_name,mnesia,async_dirty,
		[Function,Arguments]
	).


%% ----------------------------
%% @doc Return all keys from table
-spec all_keys(Node_name,Table) -> list_of_keys() | {aborted,_Reason}
	when
		Node_name :: node(),
		Table :: table_name().

all_keys(Node_name,Table) ->
	a_rpc:async_call(
		Node_name,mnesia,all_keys,
		[Table]
	).


%% ----------------------------
%% @doc Add index to the table
-spec add_table_index(Node_name,Table,Attribute) -> {aborted,_Reason} | {atomic, ok}
	when
		Node_name :: node(),
		Table :: table_name(),
		Attribute :: attribute_name().

add_table_index(Node_name,Table,Attribute) ->
	a_rpc:async_call(
		Node_name,mnesia,add_table_index,
		[Table,Attribute]
	).


%% ----------------------------
%% @doc Executes the functional object Fun with argument Arguments.
-spec activity(Node_name,Access_context,Function,Arguments,Access_module) ->
	result_of_function() | {_Reason,_Stack}
	when
		Node_name :: node(),
		Access_context :: transaction | {transaction,_Retries} | sync_transaction |
			{sync_transaction,_Retries} | async_dirty | ets,
		Function :: function(),
		Arguments :: list(),
		Access_module :: callback_module().
		
activity(Node_name,Access_context,Function,Arguments,Access_mode) ->
	a_rpc:async_call(
		Node_name,mnesia,activity,
		[Access_context,Function,Arguments,Access_mode]
	).


%% ----------------------------
%% @doc Make a checkpoint is a consistent view of the system
-spec activate_checkpoint(Node_name,Arguments) -> {ok,_Name,_Nodes} | {error,_Reason}
	when
		Node_name :: node(),
		Arguments :: {name,_Name} | {max,_MaxTabs} | {min,_MinTabs} |
			{allow_remote,_Bool} | {ram_overrides_dump,_Bool}.

activate_checkpoint(Node_name,Arguments) ->
	a_rpc:async_call(
		Node_name,mnesia,activate_checkpoint,
		[Arguments]
	).