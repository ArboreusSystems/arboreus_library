%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Data model handler: astr_link
%%%
%%% @end
%%% Created : 02. Май 2018 13:04
%%%-------------------------------------------------------------------
-module(astr_link).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Data types
-include("../include/types/types_general.hrl").
-include("../include/types/types_a_structure_tree.hrl").

%% Data models
-include("../include/records/records_a_structure_tree.hrl").

%% Constants
-include("../include/constants/a_constants_structure_tree.hrl").
-define(MODEL_NAME,?NAME_LINK).

%% API
-export([
	test/0,
	create/1,
	read/1,
	update_strength/2,
	delete/1,
	select/3,dirty_select/3
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	{equalpoints,_} = create(#astr_link{point_a = 0,point_b = 0}),
	Point = #astr_point{weight = 1,twig = 1,kind = 1,container = 1},
	{ok,Point_id1} = astr_point:create(Point),
	{ok,Point_id2} = astr_point:create(Point),
	{ok,Point_id3} = astr_point:create(Point),
	{ok,Point_id4} = astr_point:create(Point),
	Link1_income = #astr_link{point_a = Point_id1,point_b = Point_id2},
	Link2_income = #astr_link{point_a = Point_id2,point_b = Point_id3},
	Link3_income = #astr_link{point_a = Point_id1,point_b = Point_id3},
	Link4_income = #astr_link{point_a = Point_id2,point_b = Point_id4},
	{ok,Id1} = create(Link1_income),
	{ok,Id2} = create(Link2_income),
	{ok,Id3} = create(Link3_income),
	{ok,Id4} = create(Link4_income),
	{existed,Id3} = create(Link3_income),
	io:format("Ok. Creating test links finished ~n"),
	{ok,Link1} = read(Id1),
	#astr_link{id = Id1,point_a = Point_id1,point_b = Point_id2} = Link1,
	{ok,#astr_link{id = Id2,point_a = Point_id2,point_b = Point_id3}} = read(Id2),
	{ok,#astr_link{id = Id3,point_a = Point_id1,point_b = Point_id3}} = read(Id3),
	{ok,#astr_link{id = Id4,point_a = Point_id2,point_b = Point_id4}} = read(Id4),
	io:format("Ok. Reading test links finished ~n"),
	{ok,[#astr_link{id = Id1}]} = dirty_select(by_points,[Point_id1,Point_id2],return_record),
	{ok,[#astr_link{id = Id4}]} = dirty_select(by_point,[Point_id4],return_record),
	{atomic,{ok,[#astr_link{id = Id4}]}} = mnesia:transaction(fun() ->
		select(by_point,[Point_id4],return_record)
	end),
	{atomic,{ok,[#astr_link{id = Id2}]}} = mnesia:transaction(fun() ->
		select(by_points,[Point_id2,Point_id3],return_record)
	end),
	io:format("Ok. Selecting test links finished ~n"),
	Strength1 = 100, Strength2 = 200, Strength3 = 300,
	{ok,Id1} = update_strength(Strength1,Id1),
	{ok,#astr_link{id = Id1,strength = Strength1}} = read(Id1),
	{ok,Id1} = update_strength(Strength2,Link1),
	{ok,#astr_link{id = Id1,strength = Strength2}} = read(Id1),
	{ok,Id1} = update_strength(Strength3,[Point_id1,Point_id2]),
	{ok,Link1_fo_delete} = read(Id1),
	#astr_link{id = Id1,strength = Strength3} = Link1_fo_delete,
	io:format("Ok. Updating test links finished ~n"),
	{ok,Id1} = delete(Link1_fo_delete),
	{ok,Id2} = delete(Id2),
	{ok,Id3} = delete([Point_id1,Point_id3]),
	{ok,Id4} = delete(Id4),
	{ok,_} = astr_point:delete(Point_id1),
	{ok,_} = astr_point:delete(Point_id2),
	{ok,_} = astr_point:delete(Point_id3),
	{ok,_} = astr_point:delete(Point_id4),
	io:format("Ok. Deleting test links finished ~n"),
	ok.


%% ----------------------------
%% @doc Transactional select from DB
-spec select(Kind,Properties,Return_mode) ->
	{ok,_Datum} | {norow,Properties} | {aborted,_Reason}
	when
	Kind :: by_point | by_points,
	Properties :: a_list_of_properties(),
	Return_mode :: return_id | return_ids | return_record | return_records.

select(by_point,[Point],Return_mode) ->
	Match_head = #astr_link{point_a = '$1',point_b = '$2',_ = '_'},
	Guard = {'or',{'==','$1',Point},{'==','$2',Point}},
	Result = '$_',
	case mnesia:select(
		?MODEL_NAME,[{Match_head,[Guard],[Result]}]
	) of
		[] -> {norow,[Point]};
		{aborted,Reason} -> {aborted,Reason};
		Records -> select_return(Return_mode,Records)
	end;
select(by_points,[Point_a,Point_b],Return_mode) ->
	Match_head = #astr_link{point_a = '$1',point_b = '$2',_ = '_'},
	Guard = {'or',
		{'and',{'==','$1',Point_a},{'==','$2',Point_b}},
		{'and',{'==','$1',Point_b},{'==','$2',Point_a}}
	},
	Result = '$_',
	case mnesia:select(
		?MODEL_NAME,[{Match_head,[Guard],[Result]}]
	) of
		[] -> {norow,[Point_a,Point_b]};
		{aborted,Reason} -> {aborted,Reason};
		Records -> select_return(Return_mode,Records)
	end.


%% ----------------------------
%% @doc Dirty select links from DB
-spec dirty_select(Kind,Properties,Return_mode) ->
	{ok,_Datum} | {norow,Properties} | {aborted,_Reason}
	when
	Kind :: by_point | by_points,
	Properties :: a_list_of_properties(),
	Return_mode :: return_id | return_ids | return_record | return_records.

dirty_select(by_point,[Point],Return_mode) ->
	Match_head = #astr_link{point_a = '$1',point_b = '$2',_ = '_'},
	Guard = {'or',{'==','$1',Point},{'==','$2',Point}},
	Result = '$_',
	case mnesia:dirty_select(
		?MODEL_NAME,[{Match_head,[Guard],[Result]}]
	) of
		[] -> {norow,[Point]};
		{aborted,Reason} -> {aborted,Reason};
		Records -> select_return(Return_mode,Records)
	end;
dirty_select(by_points,[Point_a,Point_b],Return_mode) ->
	Match_head = #astr_link{point_a = '$1',point_b = '$2',_ = '_'},
	Guard = {'or',
		{'and',{'==','$1',Point_a},{'==','$2',Point_b}},
		{'and',{'==','$1',Point_b},{'==','$2',Point_a}}
	},
	Result = '$_',
	case mnesia:dirty_select(
		?MODEL_NAME,[{Match_head,[Guard],[Result]}]
	) of
		[] -> {norow,[Point_a,Point_b]};
		{aborted,Reason} -> {aborted,Reason};
		Records -> select_return(Return_mode,Records)
	end.


%% ----------------------------
%% @doc Select datum from record by defined field
-spec select_return(Datum,Return_mode) -> {ok,_Datum}
	when
	Datum :: astr_link() | a_list_of_records(),
	Return_mode :: return_id | return_ids | return_record | return_records.

select_return(return_id,[Record]) ->
	{ok,Record#astr_link.id};
select_return(return_ids,Records) ->
	{ok,[Record#astr_link.id || Record <- Records]};
select_return(_,Datum) -> {ok,Datum}.


%% ----------------------------
%% @doc Delete link between points
-spec delete(Link) ->
	{ok,Astr_link_id} | {norow,Astr_link_id} | {error,_Reason}
	when
	Link :: astr_link() | astr_link_id() | astr_link_points(),
	Astr_link_id :: astr_link_id().

delete([Point_a,Point_b]) ->
	case dirty_select(by_points,[Point_a,Point_b],return_record) of
		{ok,[Astr_link]} ->
			case mnesia:transaction(fun() -> mnesia:delete_object(Astr_link) end) of
				{atomic,_} -> {ok,Astr_link#astr_link.id};
				Reply -> {error,Reply}
			end;
		{aborted,Reason} -> {error,Reason};
		Reply -> Reply
	end;
delete(Astr_link) when is_record(Astr_link,astr_link) ->
	delete(Astr_link#astr_link.id);
delete(Astr_link_id) ->
	case read(Astr_link_id) of
		{ok,Astr_link} ->
			case mnesia:transaction(fun() -> mnesia:delete_object(Astr_link) end) of
				{atomic,_} -> {ok,Astr_link#astr_link.id};
				Reply -> {error,Reply}
			end;
		Reply -> Reply
	end.


%% ----------------------------
%% @doc Update the link's strength
-spec update_strength(Strength,Properties) ->
	{ok,Astr_link_id} | {norow,Astr_link_id} | {error,_Reason}
	when
	Strength :: astr_link_strength(),
	Properties :: a_list_of_properties() | astr_link() | astr_link_id(),
	Astr_link_id :: astr_link_id().

update_strength(Strength,[Point_a,Point_b]) ->
	case dirty_select(by_points,[Point_a,Point_b],return_record) of
		{ok,[Astr_link]} -> update_strength(Strength,Astr_link);
		{aborted,Reason} -> {error,Reason};
		Reply -> Reply
	end;
update_strength(Strength,Astr_link) when is_record(Astr_link,astr_link) ->
	case mnesia:transaction(fun() ->
		mnesia:write(Astr_link#astr_link{strength = Strength})
	end) of
		{atomic,_} -> {ok,Astr_link#astr_link.id};
		Reply -> {error,Reply}
	end;
update_strength(Strength,Astr_link_id) ->
	case read(Astr_link_id) of
		{ok,Astr_link} -> update_strength(Strength,Astr_link);
		Reply -> Reply
	end.


%% ----------------------------
%% @doc Read link by ID
-spec read(Astr_link_id) ->
	{norow,Astr_link_id} | {ok,Astr_link} | {error,Astr_link_id}
	when
	Astr_link_id :: astr_link_id(),
	Astr_link :: astr_link().

read(Astr_link_id) ->
	case mnesia:dirty_read(?MODEL_NAME,Astr_link_id) of
		[] -> {norow,Astr_link_id};
		[Astr_link] -> {ok,Astr_link};
		_ -> {error,Astr_link_id}
	end.
	

%% ----------------------------
%% @doc Create new link between points
-spec create(Record) ->
	{ok,astr_link_id()} | {existed,astr_link_id()} | {nopoint,astr_point_id()} |
	{error,Record} | {equalpoints,[astr_point_id()|astr_point_id()]}
	when
	Record :: astr_link().

create(Record) when is_record(Record,astr_link) ->
	case mnesia:transaction(fun() ->
		Astr_link_id = generate_id(Record#astr_link.point_a,Record#astr_link.point_b),
		case mnesia:read(?MODEL_NAME,Astr_link_id) of
			[] ->
				if
					Record#astr_link.point_a == Record#astr_link.point_b ->
						mnesia:abort({equalpoints,[
							Record#astr_link.point_a,Record#astr_link.point_b
						]});
					true ->
						case astr_point:read(Record#astr_link.point_a) of
							{ok,_} -> ok;
							_ -> mnesia:abort({nopoint,Record#astr_link.point_a})
						end,
						case astr_point:read(Record#astr_link.point_b) of
							{ok,_} -> ok;
							_ -> mnesia:abort({nopoint,Record#astr_link.point_b})
						end,
						mnesia:write(Record#astr_link{id = Astr_link_id}),
						Astr_link_id
				end;
			[Astr_link] ->
				mnesia:abort({existed,Astr_link#astr_link.id})
		end
	end) of
		{atomic,Astr_link_id} -> {ok,Astr_link_id};
		{aborted,{existed,Astr_link_id}} -> {existed,Astr_link_id};
		{aborted,{nopoint,Point}} -> {nopoint,Point};
		{aborted,{equalpoints,Points}} -> {equalpoints,Points};
		_ -> {error,Record}
	end.


%% ----------------------------
%% @doc Generate ID for link between points A and B
-spec generate_id(Point_a,Point_b) -> a_md5_binary()
	when
	Point_a :: astr_point_id(),
	Point_b :: astr_point_id().

generate_id(Point_a,Point_b) ->
	a_sequence:md(lists:sort([Point_a,Point_b]),md5).