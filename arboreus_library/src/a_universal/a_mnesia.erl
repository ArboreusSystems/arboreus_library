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
-include("a_includes.hrl").

%% API
-export([

	test/0,

	create_schema_local/0,create_schema_remotely/1,
	delete_schema_local/0,delete_schema_remotely/1,
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
-spec transaction_delete(TABLE,KEY) -> {ok,KEY} | {norow,KEY} | {error,_REASON}
	when
		TABLE :: atom(),
		KEY :: any().

transaction_delete(TABLE,KEY) ->

	case mnesia:transaction(fun() ->
		case delete(TABLE,KEY) of
			{ok,_} -> ok;
			{norow,_} -> norow;
			_ -> error
		end
	end) of
		{atomic,ok} -> {ok,KEY};
		{atomic,norow} -> {norow,KEY};
		TRANSACTION_RESULT -> {error,TRANSACTION_RESULT}
	end.


%% ----------------------------
%% @doc Transactional delete objects by key
-spec delete(TABLE,KEY) -> {norow,KEY} | {ok,KEY}
	when
		TABLE :: atom(),
		KEY :: any().

delete(TABLE,KEY) ->
	case mnesia:read(TABLE,KEY) of
		[] -> {norow,KEY};
		RECORDS ->
			{lists:foreach(fun(RECORD) ->
				mnesia:delete_object(RECORD)
			end,RECORDS),KEY}
	end.


%% ----------------------------
%% @doc Dirty delete equivalent of transaction_delete\2
-spec dirty_delete(TABLE,KEY) -> {norow,KEY} | {ok,KEY}
	when
		TABLE :: atom(),
		KEY :: any().

dirty_delete(TABLE,KEY) ->
	case mnesia:dirty_read(TABLE,KEY) of
		[] -> {norow,KEY};
		RECORDS ->
			{lists:foreach(fun(RECORD) ->
				mnesia:dirty_delete_object(RECORD)
			end,RECORDS),KEY}
	end.


%% ----------------------------
%% @doc Generate validated row ID
-spec generate_id(KIND,TABLE,DICTIONARIES,LENGTH) -> a_id()
	when
		KIND :: transactional | dirty,
		TABLE :: atom(),
		DICTIONARIES :: a_list_of_atoms(),
		LENGTH :: pos_integer().

generate_id(transactional,TABLE,DICTIONARIES,LENGTH) ->

	ID = a_sequence:random(DICTIONARIES,LENGTH),
	case mnesia:read(TABLE,ID) of
		[] -> ID;
		_ -> generate_id(transactional,TABLE,DICTIONARIES,LENGTH)
	end;

generate_id(dirty,TABLE,DICTIONARIES,LENGTH) ->

	ID = a_sequence:random(DICTIONARIES,LENGTH),
	case mnesia:dirty_read(TABLE,ID) of
		[] -> ID;
		_ -> generate_id(dirty,TABLE,DICTIONARIES,LENGTH)
	end.


%% ----------------------------
%% @doc Read transaction
-spec transaction_read(TABLE,KEY) -> {ok,RESULT} | {norow,KEY} | {aborted,REASON}
	when
		TABLE :: atom(),
		KEY :: any(),
		RESULT :: a_record() | a_list_of_records(),
		REASON :: term().

transaction_read(Table,Key) ->

	case mnesia:transaction(fun() -> read(Table,Key) end) of
		{atomic,TRANSACTION_RESULT} -> TRANSACTION_RESULT;
		RESULT -> RESULT
	end.

%% ----------------------------
%% @doc Mnesia read equivalent
-spec read(TABLE,KEY) -> {norow,KEY} | {ok,RESULT}
	when
		TABLE :: atom(),
		KEY :: any(),
		RESULT :: a_record() | a_list_of_records().

read(TABLE,KEY) ->

	case mnesia:read(TABLE,KEY) of
		[] -> {norow,KEY};
		[RECORD] -> {ok,RECORD};
		RECORDS -> {ok,RECORDS}
	end.


%% ----------------------------
%% @doc Mnesia dirty_read equivalent
-spec dirty_read(TABLE,KEY) -> {norow,KEY} | {ok,a_record()} | {ok,a_list_of_records()}
	when
	TABLE :: atom(),
	KEY :: any().

dirty_read(TABLE,KEY) ->

	case mnesia:dirty_read(TABLE,KEY) of
		[] -> {norow,KEY};
		[RECORD] -> {ok,RECORD};
		RECORDS -> {ok,RECORDS}
	end.

%% ----------------------------
%% @doc Dirty read by Ids from the table handler, wrapper for dirty_read_by_ids_handler/3
-spec dirty_read_by_ids(TABLE,IDS) -> OUTPUT
	when
		TABLE :: atom(),
		IDS :: list(),
		OUTPUT :: a_list_of_records().

dirty_read_by_ids(TABLE,IDS) ->

	dirty_read_by_ids_handler(TABLE,IDS,[]).


%% ----------------------------
%% @doc Dirty read by Ids from the table handler
-spec dirty_read_by_ids_handler(TABLE,IDS,OUTPUT) -> a_list_of_records()
	when
		TABLE :: atom(),
		IDS :: list(),
		OUTPUT :: a_list_of_records().

dirty_read_by_ids_handler(_,[],OUTPUT) -> OUTPUT;

dirty_read_by_ids_handler(TABLE,[ID|IDS],OUTPUT) ->

	dirty_read_by_ids_handler(
		TABLE,IDS,
		lists:append([OUTPUT,mnesia:dirty_read(TABLE,ID)])
	).


%% ----------------------------
%% @doc Transaction reading data by Ids from the table
-spec transaction_read_by_ids(TABLE,IDS) -> {aborted,REASON} | {atomic,OUTPUT}
	when
		TABLE :: atom(),
		IDS :: list(),
		OUTPUT :: a_list_of_records(),
		REASON :: term().

transaction_read_by_ids(TABLE,IDS) ->

	mnesia:transaction(fun() -> read_by_ids(TABLE,IDS) end).


%% ----------------------------
%% @doc Read by Ids from table, wrapper for read_by_ids_handler/3
-spec read_by_ids(TABLE,IDS) -> OUTPUT
	when
		TABLE :: atom(),
		IDS :: list(),
		OUTPUT :: a_list_of_records().

read_by_ids(TABLE,IDS) -> read_by_ids_handler(TABLE,IDS,[]).


%% ----------------------------
%% @doc Read by Ids from table handler
-spec read_by_ids_handler(TABLE,IDS,OUTPUT) -> OUTPUT
	when
		TABLE :: atom(),
		IDS :: list(),
		OUTPUT :: a_list_of_records().

read_by_ids_handler(_,[],OUTPUT) -> OUTPUT;

read_by_ids_handler(TABLE,[ID|IDS],OUTPUT) ->

	read_by_ids_handler(
		TABLE,IDS,
		lists:append([OUTPUT,mnesia:read(TABLE,ID)])
	).


%% ----------------------------
%% @doc Generate unique transaction
-spec transaction_generate_unique(RECORD,DICTIONARY,KEY_LENGTH) ->
	{aborted,REASON} | {atomic,{ok,ID}}
	when
		RECORD :: a_record(),
		DICTIONARY :: [DICTIONARY_NAME],
		DICTIONARY_NAME :: numeric | alpha_lower | alpha_upper,
		KEY_LENGTH :: pos_integer(),
		REASON :: term(),
		ID :: any().

transaction_generate_unique(RECORD,DICTIONARY,KEY_LENGTH) ->

	mnesia:transaction(fun() ->
		generate_unique(RECORD,DICTIONARY,KEY_LENGTH)
	end).


%% ----------------------------
%% @doc Transactional generate unique record in DB
-spec generate_unique(RECORD,DICTIONARY,KEY_LENGTH) -> {ok,ID}
	when
		RECORD :: a_record(),
		DICTIONARY :: [DICTIONARY_NAME],
		DICTIONARY_NAME :: numeric | alpha_lower | alpha_upper,
		KEY_LENGTH :: pos_integer(),
		ID :: any().

generate_unique(RECORD,DICTIONARY,KEY_LENGTH) ->

	ID = a_sequence:random(DICTIONARY,KEY_LENGTH),
	case create_unique(setelement(2,RECORD,ID)) of
		existed -> generate_unique(RECORD,DICTIONARY,KEY_LENGTH);
		ok -> {ok,ID}
	end.


%% ----------------------------
%% @doc Create unique transaction
-spec transaction_create_unique(RECORD) ->
	{aborted,REASON} | {atomic,ok} | {atomic,existed}
	when
		RECORD :: a_record(),
		REASON :: term().

transaction_create_unique(RECORD) ->

	mnesia:transaction(fun() -> create_unique(RECORD) end).


%% ----------------------------
%% @doc Transactional create unique record after checking previous existence
-spec create_unique(RECORD) -> ok | existed
	when RECORD :: a_record().

create_unique(RECORD) ->

	case mnesia:read(element(1,RECORD),element(2,RECORD)) of
		[] -> mnesia:write(RECORD);
		_ -> existed
	end.


%% ----------------------------
%% @doc Dirty create rows in the table, wrapper for mnesia:dirty_write/1
-spec dirty_create(DATUM) -> ok
	when DATUM :: a_list_of_records() | a_record().

dirty_create(RECORDS) when is_list(RECORDS) ->

	lists:foreach(fun(RECORD) -> mnesia:dirty_write(RECORD)	end,RECORDS);

dirty_create(RECORD) -> dirty_create([RECORD]).


%% ----------------------------
%% @doc Transactional creating row in the table
-spec transaction_create(DATUM) -> {aborted,REASON} | {atomic,RESULT}
	when
		DATUM :: a_list_of_records() | a_record(),
		REASON :: term(),
		RESULT :: any().

transaction_create(DATUM) ->

	mnesia:transaction(fun() -> create(DATUM) end).


%% ----------------------------
%% @doc Create rows in the table, wrapper for mnesia:write/1
-spec create(DATUM) -> ok
	when DATUM :: a_list_of_records() | a_record().

create(RECORDS) when is_list(RECORDS) ->

	lists:foreach(fun(RECORD) -> mnesia:write(RECORD) end,RECORDS);

create(RECORD) -> create([RECORD]).


%% ----------------------------
%% @doc Update unique transaction
-spec transaction_update_unique(RECORD) -> {atomic,{norow,ID}} | {atomic,{ok,ID}} | {aborted,REASON}
	when
		RECORD :: a_record(),
		ID :: any(),
		REASON :: term().

transaction_update_unique(Record) ->
	mnesia:transaction(fun() -> update_unique(Record) end).


%% ----------------------------
%% @doc Transactional update record in DB by new values
-spec update_unique(RECORD) -> {norow,ID} | {ok,ID}
	when
		RECORD :: a_record(),
		ID :: any().

update_unique(Record) ->

	ID = element(2,Record),
	case mnesia:read(element(1,Record),ID) of
		[] -> {norow,ID};
		[_|[]] -> {mnesia:write(Record),ID}
	end.


%% ----------------------------
%% @doc Dirty select all rows from table
-spec dirty_select_all(TABLE) -> [DATA] | {aborted,REASON}
	when
		TABLE :: atom(),
		DATA :: any(),
		REASON :: term().

dirty_select_all(TABLE) ->

	mnesia:dirty_select(TABLE,[{'_',[],['$_']}]).


%% ----------------------------
%% @doc Select all from the table transaction
-spec transaction_select_all(TABLE) -> {aborted,REASON} | {atomic,OUTPUT}
	when
		TABLE :: atom(),
		REASON :: term(),
		OUTPUT :: a_list_of_records().

transaction_select_all(TABLE) ->

	mnesia:transaction(fun() -> select_all(TABLE) end).


%% ----------------------------
%% @doc Select all rows from te table for transaction usage
-spec select_all(TABLE) -> [DATA] | {aborted,REASON}
	when
		TABLE :: atom(),
		DATA :: any(),
		REASON :: term().

select_all(TABLE) ->

	mnesia:select(TABLE,[{'_',[],['$_']}]).


%% ----------------------------
%% @doc
-spec create_schema_local() -> {ok,NODE} | {error,NODE,REASON}
	when
		NODE :: node(),
		REASON :: term().

create_schema_local() ->

	F_CREATE_SCHEMA = fun() ->
		NODE = node(),
		case mnesia:create_schema([NODE]) of
			{error,{_,{already_exists,_}}} ->
				mnesia:delete([NODE]),
				create_schema_local();
			ok ->
				{ok,NODE};
			{error,REASON} ->
				{error,NODE,REASON}
		end
	end,

	case mnesia:system_info(is_running) of
		yes ->
			mnesia:stop(),
			F_CREATE_SCHEMA();
		no ->
			F_CREATE_SCHEMA()
	end.


%% ----------------------------
%% @doc Create Mnesia DB schema on current node
-spec create_schema_remotely(NODES) -> OUTPUT
	when
		NODES :: [NODE],
		NODE :: node(),
		OUTPUT :: [NODE_REPLY],
		NODE_REPLY :: {ok,NODE} | {error,NODE,REASON},
		REASON :: term().

create_schema_remotely(NODES) -> create_schema_remotely_handler(NODES,[]).


%% ----------------------------
%% @doc Handler function for create_schema_remotely/1
-spec create_schema_remotely_handler(NODES,OUTPUT) -> OUTPUT
	when
		NODES :: [NODE],
		NODE :: node(),
		OUTPUT :: [NODE_REPLY],
		NODE_REPLY :: {ok,NODE} | {error,NODE,REASON},
		REASON :: term().

create_schema_remotely_handler([NODE|NODES],OUTPUT) ->

	create_schema_remotely_handler(
		NODES,
		lists:append(
			[a_rpc:async_call(NODE,a_mnesia,create_schema_local,[])],OUTPUT
		)
	);

create_schema_remotely_handler([],OUTPUT) -> OUTPUT.


%% ----------------------------
%% @doc Delete Mnesia DB schema on current nodes
-spec delete_schema_local() -> {ok,NODE} | {error,NODE,REASON}
	when
		NODE :: node(),
		REASON :: term().

delete_schema_local() ->

	F_DELETE_SCHEMA = fun() ->
		NODE = node(),
		case mnesia:delete_schema([NODE]) of
			ok -> {ok,NODE};
			{error,REASON} -> {error,NODE,REASON}
		end
	end,

	case mnesia:system_info(is_running) of
		yes ->
			mnesia:stop(),
			F_DELETE_SCHEMA();
		no ->
			F_DELETE_SCHEMA()
	end.


%% ----------------------------
%% @doc Delete Mnesia DB schema on defined nodes
-spec delete_schema_remotely(NODES) -> OUTPUT
	when
		NODES :: [NODE],
		NODE :: node(),
		OUTPUT :: [NODE_REPLY],
		NODE_REPLY :: {ok,NODE} | {error,NODE,REASON},
		REASON :: term().

delete_schema_remotely(NODES) -> delete_schema_remotely_handler(NODES,[]).


%% ----------------------------
%% @doc Handler function for delete_schema_remotely/1
-spec delete_schema_remotely_handler(NODES,OUTPUT) -> OUTPUT
	when
		NODES :: [NODE],
		NODE :: node(),
		OUTPUT :: [NODE_REPLY],
		NODE_REPLY :: {ok,NODE} | {error,NODE,REASON},
		REASON :: term().

delete_schema_remotely_handler([NODE|NODES],OUTPUT) ->

	create_schema_remotely_handler(
		NODES,
		lists:append(
			[a_rpc:async_call(NODE,a_mnesia,delete_schema_local,[])],OUTPUT
		)
	);

delete_schema_remotely_handler([],OUTPUT) -> OUTPUT.

