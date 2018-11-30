%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Data model handler: astr_points
%%%
%%% @end
%%% Created : 02. Май 2018 15:15
%%%-------------------------------------------------------------------
-module(astr_point).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Data types
-include("../../data_models/types/types_general.hrl").
-include("../../data_models/types/types_a_structure_tree.hrl").

%% Data models
-include("../../data_models/records/records_a_structure_tree.hrl").

%% Constants
-include("astr_constants.hrl").
-define(MODEL_NAME,?NAME_POINT).

%% API
-export([
	test/0,
	create/1,
	read/1,
	update/2,update_container/2,update_kind/2,update_weight/2,
	delete/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Point = #astr_point{weight = 1,twig = 1,kind = 1,container = 1},
	{ok,Astr_point_id1} = create(Point),
	{ok,Astr_point_id2} = create(Point),
	io:format("Ok. Creating test point finished ~n"),
	{ok,#astr_point{id = Astr_point_id1}} = read(Astr_point_id1),
	io:format("Ok. Reading test point finished ~n"),
	{ok,Astr_point_id1} = update_container(2,Astr_point_id1),
	{ok,#astr_point{id = Astr_point_id1,container = 2}} = read(Astr_point_id1),
	{ok,Astr_point_id1} = update_weight(2,Astr_point_id1),
	{ok,#astr_point{id = Astr_point_id1,weight = 2}} = read(Astr_point_id1),
	{ok,Astr_point_id1} = update_kind(2,Astr_point_id1),
	{ok,#astr_point{id = Astr_point_id1,kind = 2}} = read(Astr_point_id1),
	io:format("Ok. Reading test point finished ~n"),
	{ok,Alias_id} = astr_alias:create(#astr_alias{point = Astr_point_id1}),
	{ok,#astr_alias{point = Astr_point_id1}} = astr_alias:read(Alias_id),
	{ok,Link_id} = astr_link:create(#astr_link{point_b = Astr_point_id1,point_a = Astr_point_id2}),
	{ok,#astr_link{id = Link_id}} = astr_link:read(Link_id),
	{ok,Astr_point_id1} = delete(Astr_point_id1),
	{ok,Astr_point_id2} = delete(Astr_point_id2),
	{norow,_} = astr_alias:read(Alias_id),
	{norow,_} = astr_link:read(Link_id),
	io:format("Ok. Deleting test points finished ~n").


%% ----------------------------
%% @doc Delete point from DB
-spec delete(Point) ->
	{ok,astr_point_id()} | {norow,astr_point_id()} | {error,_Reason}
	when
	Point :: astr_point() | astr_point_id().

delete(Astr_point) when is_record(Astr_point,astr_point) ->
	delete(Astr_point#astr_point.id);
delete(Astr_point_id) ->
	case read(Astr_point_id) of
		{ok,Astr_point} ->
			case mnesia:transaction(fun() ->
				case astr_link:select(
					by_point,[Astr_point#astr_point.id],return_records
				) of
					{ok,Links} ->
						lists:foreach(fun(Link) -> mnesia:delete_object(Link) end,Links);
					_ -> ok
				end,
				case astr_alias:select(
					by_point,[Astr_point#astr_point.id],return_records
				) of
					{ok,Aliases} ->
						lists:foreach(fun(Alias) -> mnesia:delete_object(Alias) end,Aliases);
					_ -> ok
				end,
				mnesia:delete_object(Astr_point)
			end) of
				{atomic,_} -> {ok,Astr_point_id};
				Reply -> {error,Reply}
			end;
		Reply -> Reply
	end.


%% ----------------------------
%% @doc Update point's weight
-spec update_weight(Weight,Point) ->
	{ok,_Astr_point_id} | {norow,_Astr_point_id} | {error,_Reason}
	when
	Weight :: astr_point_weight(),
	Point :: astr_point() | astr_point_id().

update_weight(Weight,Point) ->
	update([{weight,Weight}],Point).


%% ----------------------------
%% @doc Update point's kind
-spec update_kind(Kind,Point) ->
	{ok,_Astr_point_id} | {norow,_Astr_point_id} | {error,_Reason}
	when
	Kind :: astr_point_kind(),
	Point :: astr_point() | astr_point_id().

update_kind(Kind,Point) ->
	update([{kind,Kind}],Point).


%% ----------------------------
%% @doc Updat container for point
-spec update_container(Container,Point) ->
	{ok,_Astr_point_id} | {norow,_Astr_point_id} | {error,_Reason}
	when
	Container :: astr_point_container(),
	Point :: astr_point() | astr_point_id().

update_container(Container,Point) ->
	update([{container,Container}],Point).


%% ----------------------------
%% @doc Update point in the DB
-spec update(Values,Point) ->
	{ok,Astr_point_id} | {norow,Astr_point_id} | {error,_Reason}
	when
	Values :: proplists:proplist(),
	Point :: astr_point() | astr_point_id(),
	Astr_point_id :: astr_point_id().

update(Values,Astr_point) when is_record(Astr_point,astr_point) ->
	case mnesia:transaction(fun() ->
		mnesia:write(Astr_point#astr_point{
			weight = case proplists:get_value(weight,Values) of
				undefined -> Astr_point#astr_point.weight;
				Weight -> Weight
			end,
			kind = case proplists:get_value(kind,Values) of
				undefined -> Astr_point#astr_point.kind;
				Kind -> Kind
			end,
			container = case proplists:get_value(container,Values) of
				undefined -> Astr_point#astr_point.container;
				Container -> Container
			end
		})
	end) of
		{atomic,_} -> {ok,Astr_point#astr_point.id};
		Reply -> {error,Reply}
	end;
update(Values,Astr_point_id) ->
	case read(Astr_point_id) of
		{ok,Astr_point} -> update(Values,Astr_point);
		Reply -> Reply
	end.


%% ----------------------------
%% @doc Read point from table
-spec read(Astr_point_id) ->
	{norow,Astr_point_id} | {ok,Astr_point} | {error,Astr_point_id}
	when
	Astr_point_id :: astr_point_id(),
	Astr_point :: astr_point().

read(Astr_point_id) when is_binary(Astr_point_id) ->
	case mnesia:dirty_read(?MODEL_NAME,Astr_point_id) of
		[] -> {norow,Astr_point_id};
		[Astr_point] -> {ok,Astr_point};
		_ -> {error,Astr_point_id}
	end.


%% ----------------------------
%% @doc Create new point
-spec create(Record) -> {ok,Astr_point_id} | {error,_Reply}
	when
	Record :: astr_point(),
	Astr_point_id :: astr_point_id().

create(Record) when is_record(Record,astr_point) ->
	case create_new_id() of
		{ok,Astr_point_id} ->
			case mnesia:transaction(fun() ->
				mnesia:write(Record#astr_point{id = Astr_point_id})
			end) of
				{atomic,_} -> {ok,Astr_point_id};
				Reply ->
					ok = mnesia:dirty_delete(?MODEL_NAME,Astr_point_id),
					{error,Reply}
			end;
		Reply -> Reply
	end.


%% ----------------------------
%% @doc Create new id for point
-spec create_new_id() -> {ok,astr_point_id()} | {error,_Reason}.

create_new_id() ->
	New_id = a_sequence:random([numeric,alpha_lower,alpha_upper],12),
	case mnesia:transaction(fun() ->
		case mnesia:read(?MODEL_NAME,New_id) of
			[] -> mnesia:write(#astr_point{id = New_id});
			[_Astr_point] -> mnesia:abort(existed);
			Reply -> Reply
		end
	end) of
		{atomic,_} -> {ok,New_id};
		{aborted,{existed,_}} -> create_new_id();
		Reply -> {error,Reply}
	end.