%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc The additional module for Mnesia data base
%%%
%%% @end
%%% Created : 28. Апр. 2018 15:29
%%%-------------------------------------------------------------------
-module(a_mnesia).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Data types
-include("../../data_models/types/types_general.hrl").

%% API
-export([
	test/0,
	create_schema/1,
	create/1,transaction_create/1,dirty_create/1,
	create_unique/1,transaction_create_unique/1,
	generate_unique/3,transaction_generate_unique/3,
	read/2,transaction_read/2,dirty_read/2,
	read_by_ids/2,transaction_read_by_ids/2,dirty_read_by_ids/2,
	update_unique/1,transaction_update_unique/1,
	select_all/1,transaction_select_all/1,dirty_select_all/1,
	delete/2,transaction_delete/2,dirty_delete/2,
	generate_id/4
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Delete objects transaction
-spec transaction_delete(Table,Key) -> {ok,Key} | {norow,Key} | {error,_Reason}
	when
	Table :: atom(),
	Key :: any().

transaction_delete(Table,Key) ->
	case mnesia:transaction(fun() ->
		case delete(Table,Key) of
			{ok,_} -> ok;
			{norow,_} -> norow;
			_ -> error
		end
	end) of
		{atomic,ok} -> {ok,Key};
		{atomic,norow} -> {norow,Key};
		Transaction_result -> {error,Transaction_result}
	end.


%% ----------------------------
%% @doc Transactional delete objects by key
-spec delete(Table,Key) -> {norow,Key} | {ok,Key}
	when
	Table :: atom(),
	Key :: any().

delete(Table,Key) ->
	case mnesia:read(Table,Key) of
		[] -> {norow,Key};
		Records ->
			{lists:foreach(fun(Record) ->
				mnesia:delete_object(Record)
			end,Records),Key}
	end.


%% ----------------------------
%% @doc Dirty delete equivalent of transaction_delete\2
-spec dirty_delete(Table,Key) -> {norow,Key} | {ok,Key}
	when
	Table :: atom(),
	Key :: any().

dirty_delete(Table,Key) ->
	case mnesia:dirty_read(Table,Key) of
		[] -> {norow,Key};
		Records ->
			{lists:foreach(fun(Record) ->
				mnesia:dirty_delete_object(Record)
			end,Records),Key}
	end.


%% ----------------------------
%% @doc Generate validated row ID
-spec generate_id(Kind,Table,Dictionaries,Length) -> id()
	when
	Kind :: transactional | dirty,
	Table :: atom(),
	Dictionaries :: list_of_atoms(),
	Length :: pos_integer().

generate_id(transactional,Table,Dictionaries,Length) ->
	Id = a_sequence:random(Dictionaries,Length),
	case mnesia:read(Table,Id) of
		[] -> Id;
		_ -> generate_id(transactional,Table,Dictionaries,Length)
	end;
generate_id(dirty,Table,Dictionaries,Length) ->
	Id = a_sequence:random(Dictionaries,Length),
	case mnesia:dirty_read(Table,Id) of
		[] -> Id;
		_ -> generate_id(dirty,Table,Dictionaries,Length)
	end.


%% ----------------------------
%% @doc Read transaction
-spec transaction_read(Table,Key) -> {ok,_Result} | {norow,Key} | {aborted,_Reason}
	when
	Table :: atom(),
	Key :: any().

transaction_read(Table,Key) ->
	case mnesia:transaction(fun() -> read(Table,Key) end) of
		{atomic,Transaction_result} -> Transaction_result;
		Result -> Result
	end.

%% ----------------------------
%% @doc Mnesia read equivalent
-spec read(Table,Key) -> {norow,Key} | {ok,Result}
	when
	Table :: atom(),
	Key :: any(),
	Result :: record() | list_of_records().

read(Table,Key) ->
	case mnesia:read(Table,Key) of
		[] -> {norow,Key};
		[Record] -> {ok,Record};
		Records -> {ok,Records}
	end.


%% ----------------------------
%% @doc Mnesia dirty_read equivalent
-spec dirty_read(Table,Key) -> {norow,Key} | {ok,record()} | {ok,list_of_records()}
	when
	Table :: atom(),
	Key :: any().

dirty_read(Table,Key) ->
	case mnesia:dirty_read(Table,Key) of
		[] -> {norow,Key};
		[Record] -> {ok,Record};
		Records -> {ok,Records}
	end.

%% ----------------------------
%% @doc Dirty read by Ids from the table handler, wrapper for dirty_read_by_ids_handler/3
-spec dirty_read_by_ids(Table,Ids) -> list_of_records()
	when
	Table :: atom(),
	Ids :: list().

dirty_read_by_ids(Table,Ids) ->
	dirty_read_by_ids_handler(Table,Ids,[]).


%% ----------------------------
%% @doc Dirty read by Ids from the table handler
-spec dirty_read_by_ids_handler(Table,Ids,Output) -> list_of_records()
	when
	Table :: atom(),
	Ids :: list(),
	Output :: list_of_records().

dirty_read_by_ids_handler(_,[],Output) -> Output;
dirty_read_by_ids_handler(Table,[Id|Ids],Output) ->
	dirty_read_by_ids_handler(
		Table,Ids,
		lists:append([Output,mnesia:dirty_read(Table,Id)])
	).


%% ----------------------------
%% @doc Transaction reading data by Ids from the table
-spec transaction_read_by_ids(Table,Ids) -> {aborted,_Reason} | {atomic,list_of_records()}
	when
	Table :: atom(),
	Ids :: list().

transaction_read_by_ids(Table,Ids) ->
	mnesia:transaction(fun() -> read_by_ids(Table,Ids) end).


%% ----------------------------
%% @doc Read by Ids from table, wrapper for read_by_ids_handler/3
-spec read_by_ids(Table,Ids) -> list_of_records()
	when
	Table :: atom(),
	Ids :: list().

read_by_ids(Table,Ids) -> read_by_ids_handler(Table,Ids,[]).


%% ----------------------------
%% @doc Read by Ids from table handler
-spec read_by_ids_handler(Table,Ids,Output) -> list_of_records()
	when
	Table :: atom(),
	Ids :: list(),
	Output :: list_of_records().

read_by_ids_handler(_,[],Output) -> Output;
read_by_ids_handler(Table,[Id|Ids],Output) ->
	read_by_ids_handler(
		Table,Ids,
		lists:append([Output,mnesia:read(Table,Id)])
	).


%% ----------------------------
%% @doc Generate unique transaction
-spec transaction_generate_unique(Record,Dictionary,Key_length) ->
	{aborted,_Reason} | {atomic,{ok,_Id}}
	when
	Record :: record(),
	Dictionary :: [Dictionary_name],
	Dictionary_name :: numeric | alpha_lower | alpha_upper,
	Key_length :: pos_integer().

transaction_generate_unique(Record,Dictionary,Key_length) ->
	mnesia:transaction(fun() ->
		generate_unique(Record,Dictionary,Key_length)
	end).


%% ----------------------------
%% @doc Transactional generate unique record in DB
-spec generate_unique(Record,Dictionary,Key_length) -> {ok,_Id}
	when
	Record :: record(),
	Dictionary :: [Dictionary_name],
	Dictionary_name :: numeric | alpha_lower | alpha_upper,
	Key_length :: pos_integer().

generate_unique(Record,Dictionary,Key_length) ->
	Id = a_sequence:random(Dictionary,Key_length),
	case create_unique(setelement(2,Record,Id)) of
		existed -> generate_unique(Record,Dictionary,Key_length);
		ok -> {ok,Id}
	end.


%% ----------------------------
%% @doc Create unique transaction
-spec transaction_create_unique(Record) -> {aborted,_Reason} | {atomic,ok} | {atomic,existed}
	when
	Record :: record().

transaction_create_unique(Record) ->
	mnesia:transaction(fun() -> create_unique(Record) end).


%% ----------------------------
%% @doc Transactional create unique record after checking previous existence
-spec create_unique(Record) -> ok | existed
	when
	Record :: record().

create_unique(Record) ->
	case mnesia:read(element(1,Record),element(2,Record)) of
		[] -> mnesia:write(Record);
		_ -> existed
	end.


%% ----------------------------
%% @doc Dirty create rows in the table, wrapper for mnesia:dirty_write/1
-spec dirty_create(Datum) -> ok
	when
	Datum :: list_of_records() | record().

dirty_create(Records) when is_list(Records) ->
	lists:foreach(fun(Record) -> mnesia:dirty_write(Record)	end,Records);
dirty_create(Record) -> dirty_create([Record]).


%% ----------------------------
%% @doc Transactional creating row in the table
-spec transaction_create(Datum) -> {aborted,_Reason} | {atomic,_ResultOfFun}
	when
	Datum :: list_of_records() | record().

transaction_create(Datum) ->
	mnesia:transaction(fun() -> create(Datum) end).


%% ----------------------------
%% @doc Create rows in the table, wrapper for mnesia:write/1
-spec create(Datum) -> ok
	when
	Datum :: list_of_records() | record().

create(Records) when is_list(Records) ->
	lists:foreach(fun(Record) -> mnesia:write(Record) end,Records);
create(Record) -> create([Record]).


%% ----------------------------
%% @doc Update unique transaction
-spec transaction_update_unique(Record) -> {atomic,{norow,_Id}} | {atomic,{ok,_Id}} | {aborted,_Reason}
	when
	Record :: record().

transaction_update_unique(Record) ->
	mnesia:transaction(fun() -> update_unique(Record) end).


%% ----------------------------
%% @doc Transactional update record in DB by new values
-spec update_unique(Record) -> {norow,_} | {ok,_}
	when
	Record :: record().

update_unique(Record) ->
	Id = element(2,Record),
	case mnesia:read(element(1,Record),Id) of
		[] -> {norow,Id};
		[_|[]] -> {mnesia:write(Record),Id}
	end.


%% ----------------------------
%% @doc Dirty select all rows from table
-spec dirty_select_all(Table) -> [_Datum] | {aborted,_Reason}
	when
	Table :: atom().

dirty_select_all(Table) ->
	mnesia:dirty_select(Table,[{'_',[],['$_']}]).


%% ----------------------------
%% @doc Select all from the table transaction
-spec transaction_select_all(Table) -> {aborted,_Reason} | {atomic,list_of_records()}
	when
	Table :: atom().

transaction_select_all(Table) ->
	mnesia:transaction(fun() -> select_all(Table) end).


%% ----------------------------
%% @doc Select all rows from te table for transaction usage
-spec select_all(Table) -> [_Datum] | {aborted,_Reason}
	when
	Table :: atom().

select_all(Table) ->
	mnesia:select(Table,[{'_',[],['$_']}]).


%% ----------------------------
%% @doc Create schema on the defined nodes
-spec create_schema(Nodes) -> ok | error
	when
	Nodes :: [node()].

create_schema(Nodes) ->
	create_schema_handler(mnesia_check,Nodes).


%% ----------------------------
%% @doc Create schema on the defined nodes
-spec create_schema_handler(Action,Nodes) -> ok | error
	when
	Action :: mnesia_check | create | rebuild,
	Nodes :: [node()].

create_schema_handler(mnesia_check,Nodes) ->
	case mnesia:system_info(is_running) of
		yes ->
			mnesia:stop(),
			create_schema_handler(create,Nodes);
		no ->
			create_schema_handler(create,Nodes)
	end;
create_schema_handler(create,Nodes) ->
	ok = create_schema_handler(rebuild,Nodes),
	ok = mnesia:start();
create_schema_handler(rebuild,Nodes) ->
	case mnesia:create_schema(Nodes) of
		{error,{_,{already_exists,_}}} ->
			mnesia:delete_schema(Nodes),
			mnesia:create_schema(Nodes);
		ok -> ok;
		_ -> error
	end.
