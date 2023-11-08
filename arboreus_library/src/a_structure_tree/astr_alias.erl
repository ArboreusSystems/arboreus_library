%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Data model handler: astr_alias
%%%
%%% @end
%%% Created : 02. Май 2018 11:44
%%%-------------------------------------------------------------------
-module(astr_alias).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Data types
-include("../include/types/types_general.hrl").
-include("../include/types/types_a_structure_tree.hrl").

%% Data models
-include("../include/records/records_a_structure_tree.hrl").

%% Constants
-include("../include/constants/a_constants_structure_tree.hrl").
-define(MODEL_NAME,?NAME_ALIAS).

%% API
-export([
	test/0,
	create/1,
	read/1,
	update/2,update_point/2,update_description/2,
	delete/1,
	select/3,dirty_select/3
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Alias1 = #astr_alias{alias = <<("test")/utf8>>,point = "test",description = "test"},
	{nopoint,_} = create(Alias1),
	Point1 = #astr_point{weight = 1, twig = "test",kind = "test",container = "test"},
	{ok,Point_id1} = astr_point:create(Point1),
	{ok,Point_id2} = astr_point:create(Point1),
	{ok,Alias_id} = create(Alias1#astr_alias{point = Point_id1}),
	{existed,_} = create(Alias1#astr_alias{point = Point_id1}),
	io:format("Ok. Creating test aliases finished ~n"),
	{ok,_} = read(Alias_id),
	io:format("Ok. Reading test aliases finished ~n"),
	{atomic,{ok,[#astr_alias{point = Point_id1,alias = Alias_id}]}} =
		mnesia:transaction(fun() ->
			select(by_point,[Point_id1],return_record)
		end),
	{ok,[#astr_alias{point = Point_id1,alias = Alias_id}]} =
		dirty_select(
			by_point,[Point_id1],return_record
		),
	io:format("Ok. Selecting test aliases finished ~n"),
	{ok,Alias_id} = update_point(Point_id2,Alias_id),
	{ok,Astr_alias} = read(Alias_id),
	#astr_alias{point = Point_id2} = Astr_alias,
	{nopoint,_} = update_point(nopoint,Alias_id),
	io:format("Ok. Updating test aliases finished ~n"),
	{ok,_} = delete(Astr_alias),
	{ok,_} = astr_point:delete(Point_id1),
	{ok,_} = astr_point:delete(Point_id2),
	io:format("Ok. Deleting test aliases finished ~n"),
	ok.


%% ----------------------------
%% @doc Dirty alternative for select/3
-spec dirty_select(Kind,Properties,Return_mode) ->
	{ok,_Datum} | {norow,Properties} | {aborted,_Reason}
	when
	Kind :: by_point,
	Properties :: list_of_properties(),
	Return_mode :: return_ids | return_records | return_record.

dirty_select(run,[Properties,Match_head,Guard,Result],Return_mode) ->
	case mnesia:dirty_select(
		?MODEL_NAME,[{Match_head,Guard,Result}]
	) of
		[] -> {norow,Properties};
		{aborted,Reason} -> {aborted,Reason};
		Records -> select_return(Return_mode,Records)
	end;
dirty_select(by_point,[Point],Return_mode) ->
	Match_head = #astr_alias{point = '$1',_ = '_'},
	Guard = [{'==','$1',Point}],
	Result = ['$_'],
	dirty_select(run,[[Point],Match_head,Guard,Result],Return_mode).


%% ----------------------------
%% @doc Do selection from Db by defined parameters
-spec select(Kind,Properties,Return_mode) ->
	{ok,_Datum} | {norow,Properties} | {aborted,_Reason}
	when
	Kind :: by_point,
	Properties :: list_of_properties(),
	Return_mode :: return_ids | return_records | return_record.

select(run,[Properties,Match_head,Guard,Result],Return_mode) ->
	case mnesia:select(
		?MODEL_NAME,[{Match_head,Guard,Result}]
	) of
		[] -> {norow,Properties};
		{aborted,Reason} -> {aborted,Reason};
		Records -> select_return(Return_mode,Records)
	end;
select(by_point,[Point],Return_mode) ->
	Match_head = #astr_alias{point = '$1',_ = '_'},
	Guard = [{'==','$1',Point}],
	Result = ['$_'],
	select(run,[[Point],Match_head,Guard,Result],Return_mode).


%% ----------------------------
%% @doc Select datum from record by defined field, additional for select/3
-spec select_return(Datum,Return_mode) -> {ok,_Datum}
	when
	Datum :: astr_alias() | list_of_records(),
	Return_mode :: return_ids | return_records | return_record.

select_return(return_ids,Records) ->
	{ok,[Record#astr_point.id || Record <- Records]};
select_return(_,Datum) -> {ok,Datum}.


%% ----------------------------
%% @doc Delete alias from DB
-spec delete(Alias) ->
	{ok,Astr_alias_id} | {norow,Astr_alias_id} | {error,Astr_alias_id}
	when
	Alias :: astr_alias() | astr_alias_id(),
	Astr_alias_id :: astr_alias_id().

delete(Record) when is_record(Record,astr_alias) ->
	delete(Record#astr_alias.alias);
delete(Astr_alias_id) ->
	case mnesia:transaction(fun() ->
		case mnesia:read(?MODEL_NAME,Astr_alias_id) of
			[] -> mnesia:abort({norow,Astr_alias_id});
			[_Astr_alias] -> mnesia:delete({?MODEL_NAME,Astr_alias_id})
		end
	end) of
		{atomic,_} ->
			case read(Astr_alias_id) of
				{norow,_} -> {ok,Astr_alias_id};
				_ -> {error,Astr_alias_id}
			end
	end.


%% ----------------------------
%% @doc Update description for alias
-spec update_description(Description,Record) ->
	{ok,astr_alias_id()} | {nopoint,astr_point_id()} | {error,astr_alias_id()}
	when
	Description :: astr_alias_description(),
	Record :: astr_alias().

update_description(Description,Record) -> update([{description,Description}],Record).


%% ----------------------------
%% @doc Update point for alias
-spec update_point(Point,Record) ->
	{ok,astr_alias_id()} | {nopoint,astr_point_id()} | {error,astr_alias_id()}
	when
	Point :: astr_point_id(),
	Record :: astr_alias().
	
update_point(Point,Record) -> update([{point,Point}],Record).


%% ----------------------------
%% @doc Update alias in the DB
-spec update(Values,Astr_alias) ->
	{ok,astr_alias_id()} | {nopoint,astr_point_id()} | {error,astr_alias_id()} | {norow,astr_alias_id()}
	when
	Values :: proplists:proplist(),
	Astr_alias :: astr_alias() | astr_alias_id().

update(Values,Record) when is_record(Record,astr_alias) ->
	case mnesia:transaction(fun() ->
		mnesia:write(Record#astr_alias{
			point = case proplists:get_value(point,Values) of
				undefined -> Record#astr_alias.point;
				Point ->
					case mnesia:read(?NAME_POINT,Point) of
						[] -> mnesia:abort({nopoint,Point});
						[_Astr_points] -> Point
					end
			end,
			description = case proplists:get_value(description,Values) of
				undefined -> Record#astr_alias.description;
				Description -> Description
			end
		})
	end) of
		{atomic,_ResultOfFun} -> {ok,Record#astr_alias.alias};
		{aborted,{nopoint,Point}} -> {nopoint,Point};
		_ -> {error,Record#astr_alias.alias}
	end;
update(Values,Astr_alias_id) ->
	case read(Astr_alias_id) of
		{ok,Astr_alias} -> update(Values,Astr_alias);
		Reply -> Reply
	end.


%% ----------------------------
%% @doc Read alias by ID
-spec read(Astr_alias_id) ->
	{norow,Astr_alias_id} | {ok,Astr_alias} | {error,Astr_alias_id}
	when
	Astr_alias_id :: astr_alias_id(),
	Astr_alias :: astr_alias().

read(Astr_alias_id) ->
	case mnesia:dirty_read(?MODEL_NAME,Astr_alias_id) of
		[] -> {norow,Astr_alias_id};
		[Astr_alias] -> {ok,Astr_alias};
		_ -> {error,Astr_alias_id}
	end.


%% ----------------------------
%% @doc Create new alias for structure point
-spec create(Record) ->
	{ok,_Astr_alias} | {existed,Astr_alias} | {aborted,_Reason}
	when
	Record :: astr_alias(),
	Astr_alias :: astr_alias().

create(Record) when is_record(Record,astr_alias) ->
	case mnesia:transaction(fun() ->
		case mnesia:read(?NAME_POINT,Record#astr_alias.point) of
			[_Astr_tree_point] ->
				case mnesia:read(?MODEL_NAME,Record#astr_alias.alias) of
					[] ->
						case mnesia:write(Record) of
							ok -> ok;
							Result_of_write -> mnesia:abort(Result_of_write)
						end;
					[Astr_alias] -> mnesia:abort({existed,Astr_alias#astr_alias.alias});
					Result_of_read -> mnesia:abort(Result_of_read)
				end;
			[] -> mnesia:abort({nopoint,Record#astr_alias.point});
			Result_of_point_read -> mnesia:abort(Result_of_point_read)
		end
	end) of
		{atomic,_ResultOfFun} -> {ok,Record#astr_alias.alias};
		{aborted,{existed,Astr_alias}} -> {existed,Astr_alias};
		{aborted,{nopoint,Point}} -> {nopoint,Point};
		Result_of_transaction -> Result_of_transaction
	end.