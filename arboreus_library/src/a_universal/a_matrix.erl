%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus matrix handler
%%%
%%% @end
%%% Created : 06/19/2018 at 21:43
%%%-------------------------------------------------------------------
-module(a_matrix).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants
-define(TEST_RECORD_NAME,test_matrix).

%% Data types
-include_lib("../include/types/types_general.hrl").

%% Data models
-record(test_matrix,{a,b,c,d}).

%% API
-export([
	test/0,
	model/2,
	verify/2,verify/3,
	sum/2,
	percentage/2
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_matrix) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	
	Description = [a,b,c,d],
	Length = 4,
	Model_list = model(list,Length),
	Model_tuple = model(tuple,Length),
	Model_record = model(record,{?TEST_RECORD_NAME,Length}),
	Model_proplist = model(proplist,Description),
	Model_map = model(map,Description),
	Model_gb = model(gb_tree,Description),
	
	Matrix_list1 = [1,1,1,1],
	Matrix_list2 = [2,2,2,2],
	Matrix_list3 = [3,3,3,3],
	Matrix_sum_list = [6,6,6,6],
	Matrix_list_wrong = [1,1,1,one],
	Matrices_list = [Matrix_list1,Matrix_list2,Matrix_list3],
	Matrices_list_wrong = [Matrix_list1,Matrix_list2,Matrix_list_wrong],
	{true,Matrix_list1} = verify(list,Model_list,Matrix_list1),
	{true,Matrix_list2} = verify(list,Model_list,Matrix_list2),
	{true,Matrix_list3} = verify(list,Model_list,Matrix_list3),
	false = verify(list,Model_list,Matrix_list_wrong),
	{true,Matrices_list} = verify(lists,Matrices_list),
	false = verify(lists,Matrices_list_wrong),
	{true,Matrix_sum_list} = sum(lists,Matrices_list),
	io:format("DONE! Testing list based matrixes finished~n"),
	
	Matrix_tuple1 = {1,1,1,1},
	Matrix_tuple2 = {2,2,2,2},
	Matrix_tuple3 = {3,3,3,3},
	Matrix_sum_tuple = {6,6,6,6},
	Matrix_tuple_wrong = {1,5,5,one},
	Matrices_tuple = [Matrix_tuple1,Matrix_tuple2,Matrix_tuple3],
	Matrices_tuple_wrong = [Matrix_tuple1,Matrix_tuple2,Matrix_tuple_wrong],
	{true,Matrix_tuple1} = verify(tuple,Model_tuple,Matrix_tuple1),
	{true,Matrix_tuple2} = verify(tuple,Model_tuple,Matrix_tuple2),
	{true,Matrix_tuple3} = verify(tuple,Model_tuple,Matrix_tuple3),
	false = verify(tuple,Model_tuple,Matrix_tuple_wrong),
	{true,Matrices_tuple} = verify(tuples,Matrices_tuple),
	false = verify(tuples,Matrices_tuple_wrong),
	{true,Matrix_sum_tuple} = sum(tuples,Matrices_tuple),
	io:format("DONE! Testing tuple based matrixes finished~n"),
	
	Matrix_record1 = #test_matrix{a = 1,b = 1,c = 1,d = 1},
	Matrix_record2 = #test_matrix{a = 2,b = 2,c = 2,d = 2},
	Matrix_record3 = #test_matrix{a = 3,b = 3,c = 3,d = 3},
	Matrix_sum_record = #test_matrix{a = 6,b = 6,c = 6,d = 6},
	Matrix_record_wrong = #test_matrix{a = 2,b = 2,c = 2,d = one},
	Matrices_record = [Matrix_record1,Matrix_record2,Matrix_record3],
	Matrices_record_wrong = [Matrix_record1,Matrix_record2,Matrix_record_wrong],
	{true,Matrix_record1} = verify(record,Model_record,Matrix_record1),
	{true,Matrix_record2} = verify(record,Model_record,Matrix_record2),
	{true,Matrix_record3} = verify(record,Model_record,Matrix_record3),
	false = verify(record,Model_record,Matrix_record_wrong),
	{true,Matrices_record} = verify(records,Matrices_record),
	false = verify(records,Matrices_record_wrong),
	{true,Matrix_sum_record} = sum(records,Matrices_record),
	io:format("DONE! Testing record based matrixes finished~n"),
	
	Matrix_proplist1 = [{a,1},{b,1},{c,1},{d,1}],
	Matrix_proplist2 = [{a,2},{b,2},{c,2},{d,2}],
	Matrix_proplist3 = [{a,3},{b,3},{c,3},{d,3}],
	Matrix_sum_proplist = [{a,6},{b,6},{c,6},{d,6}],
	Matrix_proplist_wrong = [{a,2},{b,2},{c,2},{d,one}],
	Matrices_proplist = [Matrix_proplist1,Matrix_proplist2,Matrix_proplist3],
	Matrices_proplist_wrong = [Matrix_proplist1,Matrix_proplist2,Matrix_proplist_wrong],
	{true,Matrix_proplist1} = verify(proplist,Model_proplist,Matrix_proplist1),
	{true,Matrix_proplist2} = verify(proplist,Model_proplist,Matrix_proplist2),
	{true,Matrix_proplist3} = verify(proplist,Model_proplist,Matrix_proplist3),
	false = verify(proplist,Model_proplist,Matrix_proplist_wrong),
	{true,Matrices_proplist} = verify(proplists,Matrices_proplist),
	false = verify(proplists,Matrices_proplist_wrong),
	{true,Matrix_sum_proplist} = sum(proplists,Matrices_proplist),
	io:format("DONE! Testing proplist based matrixes finished~n"),
	
	Matrix_map1 = maps:from_list(Matrix_proplist1),
	Matrix_map2 = maps:from_list(Matrix_proplist2),
	Matrix_map3 = maps:from_list(Matrix_proplist3),
	Matrix_sum_map = maps:from_list(Matrix_sum_proplist),
	Matrix_map_wrong = maps:from_list(Matrix_proplist_wrong),
	Matrices_map = [Matrix_map1,Matrix_map2,Matrix_map3],
	Matrices_map_wrong = [Matrix_map1,Matrix_map2,Matrix_map_wrong],
	{true,Matrix_map1} = verify(map,Model_map,Matrix_map1),
	{true,Matrix_map2} = verify(map,Model_map,Matrix_map2),
	{true,Matrix_map3} = verify(map,Model_map,Matrix_map3),
	false = verify(map,Model_map,Matrix_map_wrong),
	{true,Matrices_map} = verify(maps,Matrices_map),
	false = verify(maps,Matrices_map_wrong),
	{true,Matrix_sum_map} = sum(maps,Matrices_map),
	io:format("DONE! Testing map based matrixes finished~n"),
	
	Matrix_gb1 = gb_trees:from_orddict(Matrix_proplist1),
	Matrix_gb2 = gb_trees:from_orddict(Matrix_proplist2),
	Matrix_gb3 = gb_trees:from_orddict(Matrix_proplist3),
	Matrix_sum_gb = gb_trees:from_orddict(Matrix_sum_proplist),
	Matrix_gb_wrong = gb_trees:from_orddict(Matrix_proplist_wrong),
	Matrices_gb = [Matrix_gb1,Matrix_gb2,Matrix_gb3],
	Matrices_gb_wrong = [Matrix_gb1,Matrix_gb2,Matrix_gb_wrong],
	{true,Matrix_gb1} = verify(gb_tree,Model_gb,Matrix_gb1),
	{true,Matrix_gb2} = verify(gb_tree,Model_gb,Matrix_gb2),
	{true,Matrix_gb3} = verify(gb_tree,Model_gb,Matrix_gb3),
	false = verify(gb_tree,Model_gb,Matrix_gb_wrong),
	{true,Matrices_gb} = verify(gb_trees,Matrices_gb),
	false = verify(gb_trees,Matrices_gb_wrong),
	{true,Matrix_sum_gb} = sum(gb_trees,Matrices_gb),
	io:format("DONE! Testing binary trees based matrixes finished~n"),
	
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_matrix) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc

percentage(Kind,Matrices) ->
	case verify(Kind,Matrices) of
		{true,Matrices} ->
			percentage_list_handler(
				case Kind of
					lists -> a_numbers_l;
					tuples -> a_numbers_t;
					records -> a_numbers_r;
					proplists -> a_numbers_pl;
					maps -> a_numbers_m;
					gb_trees -> a_numbers_gb
				end,
				a_list:numerate(Matrices)
			);
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc

percentage_list_handler(Module,Numerated_matrices) ->
	Matrices = [Matrix || {_,Matrix} <- Numerated_matrices],
	io:format("Matrices: ~p~n",[Matrices]),
	Values = a_structure_l:plain_rotate(cw,Matrices),
	io:format("Values: ~p~n",[Values]),
	[Module:average_percentage(Value) || Value <- Values].
	

%% ----------------------------
%% @doc

sum(gb_trees,Matrices) -> sum_gb_trees(Matrices);
sum(maps,Matrices) -> sum_maps(Matrices);
sum(proplists,Matrices) -> sum_proplists(Matrices);
sum(records,Matrices) -> sum_records(Matrices);
sum(tuples,Matrices) -> sum_tuples(Matrices);
sum(lists,Matrices) -> sum_lists(Matrices).


%% ----------------------------
%% @doc Summing up gb_tree based matrices
-spec sum_gb_trees(Matrices) -> false | {true,Sum}
	when
	Matrices :: a_list_of_gb_trees(),
	Sum :: gb_trees:tree().

sum_gb_trees(Matrices) ->
	case verify(gb_trees,Matrices) of
		{true,Matrices} -> sum_handler(gb_trees,Matrices,start);
		Verification_result -> Verification_result
	end.
	

%% ----------------------------
%% @doc Summing up map based matrices
-spec sum_maps(Matrices) -> false | {true,Sum}
	when
	Matrices :: a_list_of_maps(),
	Sum :: map().

sum_maps(Matrices) ->
	case verify(maps,Matrices) of
		{true,Matrices} -> sum_handler(maps,Matrices,start);
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc Summing up proplist based matrices
-spec sum_proplists(Matrices) -> false | {true,Sum}
	when
	Matrices :: a_list_of_proplists(),
	Sum :: proplists:proplist().

sum_proplists(Matrices) ->
	case verify(proplists,Matrices) of
		{true,Matrices} -> sum_handler(proplists,Matrices,start);
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc Summing up record based procedures
-spec sum_records(Matrices) -> false | {true,Sum}
	when
	Matrices :: a_list_of_records(),
	Sum :: a_record().

sum_records(Matrices) ->
	case verify(records,Matrices) of
		{true,Matrices} -> sum_handler(records,Matrices,start);
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc Summing up tuple based matrices
-spec sum_tuples(Matrices) -> false | {true,Sum}
	when
	Matrices :: a_list_of_tuples(),
	Sum :: tuple().

sum_tuples(Matrices) ->
	case verify(tuples,Matrices) of
		{true,Matrices} -> sum_handler(tuples,Matrices,start);
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc Return sum of list based matrices
-spec sum_lists(Matrices) -> {true,Sum} | false
	when
	Matrices :: list(),
	Sum :: a_list_of_values().

sum_lists(Matrices) ->
	case verify(lists,Matrices) of
		{true,Matrices} -> sum_handler(lists,Matrices,start);
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc Proxy to specialised summing up 2 matrices procedure
-spec sum_2_matrices(Kind,Matrix1,Matrix2) -> Result
	when
	Kind :: lists | tuples | records | proplists | maps | gb_trees,
	Matrix1 :: Matrix2,
	Matrix2 :: list() | proplists:proplist() | a_record() | tuple() | map() | gb_trees:tree(),
	Result :: list() | proplists:proplist() | a_record() | tuple() | map() | gb_trees:tree().

sum_2_matrices(lists,List1,List2) -> sum_2_lists(List1,List2);
sum_2_matrices(tuples,Tuple1,Tuple2) -> sum_2_tuples(Tuple1,Tuple2,start);
sum_2_matrices(records,Record1,Record2) -> sum_2_records(Record1,Record2,start);
sum_2_matrices(proplists,Proplist1,Proplist2) -> sum_2_proplists(Proplist1,Proplist2);
sum_2_matrices(maps,Map1,Map2) -> sum_2_maps(maps:keys(Map1),Map1,Map2);
sum_2_matrices(gb_trees,Tree1,Tree2) -> sum_2_gb_trees(gb_trees:keys(Tree1),Tree1,Tree2).


%% ----------------------------
%% @doc Summing up matrices procedure handler
-spec sum_handler(Kind,Matrices,Sum) -> {true,Sum}
	when
	Kind :: lists | tuples | records | proplists | maps | gb_trees,
	Matrices :: list(),
	Sum :: list() | proplists:proplist() | a_record() | tuple() | map() | gb_trees:tree().

sum_handler(_,[],Sum) -> {true,Sum};
sum_handler(Kind,[Matrix|Matrices],start) ->
	sum_handler(Kind,Matrices,Matrix);
sum_handler(Kind,[Matrix|Matrices],Sum) ->
	sum_handler(Kind,Matrices,sum_2_matrices(Kind,Matrix,Sum)).


%% ----------------------------
%% @doc Summing up 2 gb_trees
-spec sum_2_gb_trees(Keys,Tree1,Tree2) -> gb_trees:tree()
	when
	Keys :: a_list_of_values(),
	Tree1 :: gb_trees:tree(),
	Tree2 :: gb_trees:tree().

sum_2_gb_trees([],_,Sum) -> Sum;
sum_2_gb_trees([Key|Keys],Tree1,Tree2) ->
	sum_2_gb_trees(
		Keys,Tree1,
		gb_trees:update(Key,gb_trees:get(Key,Tree1) + gb_trees:get(Key,Tree2),Tree2)
	).


%% ----------------------------
%% @doc Summing up 2 maps
-spec sum_2_maps(Keys,Map1,Map2) -> map()
	when
	Keys :: a_list_of_values(),
	Map1 :: map(),
	Map2 :: map().

sum_2_maps([],_,Sum) -> Sum;
sum_2_maps([Key|Keys],Map1,Map2) ->
	sum_2_maps(
		Keys,Map1,
		maps:update(Key,maps:get(Key,Map1) + maps:get(Key,Map2),Map2)
	).


%% ----------------------------
%% @doc Summing u 2 proplists
-spec sum_2_proplists(Proplist1,Proplist2) -> proplists:proplist()
	when
	Proplist1 :: proplists:proplist(),
	Proplist2 :: proplists:proplist().

sum_2_proplists([],Sum) -> Sum;
sum_2_proplists([{Key,Value1}|Proplist1],Proplist2) ->
	sum_2_proplists(
		Proplist1,
		lists:keyreplace(
			Key,1,Proplist2,{Key,Value1 + proplists:get_value(Key,Proplist2)}
	)).


%% ----------------------------
%% @doc Summing up 2 record based matrices
-spec sum_2_records(Record1,Record2,Count) -> a_record()
	when
	Record1 :: a_record(),
	Record2 :: a_record(),
	Count :: start | pos_integer().

sum_2_records(_,Sum,1) -> Sum;
sum_2_records(Record1,Record2,start) ->
	sum_2_records(Record1,Record2,tuple_size(Record1));
sum_2_records(Record1,Record2,Count) ->
	sum_2_records(
		Record1,
		setelement(Count,Record2,element(Count,Record1) + element(Count,Record2)),
		Count - 1
	).


%% ----------------------------
%% @doc Summing up 2 tuples
-spec sum_2_tuples(Tuple1,Sum,Count) -> Sum
	when
	Tuple1 :: tuple(),
	Sum :: tuple(),
	Count :: pos_integer() | start.

sum_2_tuples(_,Sum,0) -> Sum;
sum_2_tuples(Tuple1,Tuple2,start) ->
	sum_2_tuples(Tuple1,Tuple2,tuple_size(Tuple1));
sum_2_tuples(Tuple1,Tuple2,Count) ->
	sum_2_tuples(
		Tuple1,
		setelement(Count,Tuple2,element(Count,Tuple1) + element(Count,Tuple2)),
		Count - 1
	).


%% ----------------------------
%% @doc Summing up 2 lists
-spec sum_2_lists(List1,List2) -> list()
	when
	List2 :: list(),
	List1 :: list().

sum_2_lists(List1,List2) ->
	lists:zipwith(fun(X,Y) -> X + Y end,List1,List2).


%% ----------------------------
%% @doc

verify(gb_trees,Matrices) ->
	[Etalon|_] = Matrices,
	mass_verify(
		gb_trees,model(gb_tree,gb_trees:keys(Etalon)),Matrices
	);
verify(maps,Matrices) ->
	[Etalon|_] = Matrices,
	mass_verify(
		maps,model(map,maps:keys(Etalon)),Matrices
	);
verify(proplists,Matrices) ->
	[Etalon|_] = Matrices,
	mass_verify(
		proplists,model(proplist,proplists:get_keys(Etalon)),Matrices
	);
verify(records,Matrices) ->
	[Etalon|_] = Matrices,
	mass_verify(
		records,model(record,{element(1,Etalon),tuple_size(Etalon) - 1}),Matrices
	);
verify(tuples,Matrices) ->
	[Etalon|_] = Matrices,
	mass_verify(
		tuples,model(tuple,tuple_size(Etalon)),Matrices
	);
verify(lists,Matrices) ->
	[Etalon|_] = Matrices,
	mass_verify(
		lists,model(list,length(Etalon)),Matrices
	).


%% ----------------------------
%% @doc Matrices massive verification
-spec mass_verify(Kind,Model,Matrices) -> {true,Matrices} | false
	when
	Kind :: lists | tuples | records | proplists | maps | gb_trees,
	Model :: list() | tuple() | a_record() | proplists:proplist() | map() | gb_trees:tree(),
	Matrices :: list().

mass_verify(Kind,Model,Matrices) ->
	mass_verify_handler(Kind,Model,Matrices,[]).


%% ----------------------------
%% @doc Matrices massive verification procedure handler
-spec mass_verify_handler(Kind,Model,Matrices,Output) -> {true,Matrices} | false
	when
	Kind :: lists | tuples | records | proplists | maps | gb_trees,
	Model :: list() | tuple() | a_record() | proplists:proplist() | map() | gb_trees:tree(),
	Matrices :: list(),
	Output :: list().

mass_verify_handler(_,_,[],Output) -> {true,Output};
mass_verify_handler(Kind,Model,[Matrix|Matrices],Output) ->
	case verify(
		case Kind of
			lists -> list;
			tuples -> tuple;
			records -> record;
			proplists -> proplist;
			maps -> map;
			gb_trees -> gb_tree
		end,
		Model,Matrix
	) of
		{true,Matrix} ->
			mass_verify_handler(
				Kind,Model,Matrices,
				lists:append(Output,[Matrix])
			);
		_ -> false
	end.


%% ----------------------------
%% @doc Verify matrix
-spec verify(Kind,Model,Matrix) -> {true,Matrix} | false
	when
	Kind :: list | tuple | record | proplist | map | gb_tree,
	Model :: list() | tuple() | a_record() | proplists:proplist() | map() | gb_trees:tree(),
	Matrix :: list() | tuple() | a_record() | proplists:proplist() | map() | gb_trees:tree().

verify(list,Model,Matrix) -> verify_handler(a_structure_l,Model,Matrix);
verify(tuple,Model,Matrix) -> verify_handler(a_structure_t,Model,Matrix);
verify(record,Model,Matrix) -> verify_handler(a_structure_r,Model,Matrix);
verify(proplist,Model,Matrix) -> verify_handler(a_structure_pl,Model,Matrix);
verify(map,Model,Matrix) -> verify_handler(a_structure_m,Model,Matrix);
verify(gb_tree,Model,Matrix) -> verify_handler(a_structure_gb,Model,Matrix).


%% ----------------------------
%% @doc Matrix verification procedure handler
-spec verify_handler(Handler,Model,Matrix) -> {true,Matrix} | false
	when
	Handler :: a_structure_l | a_structure_t | a_structure_r | a_structure_pl |
		a_structure_m | a_structure_gb,
	Model :: list() | tuple() | a_record() | proplists:proplist() | map() | gb_trees:tree(),
	Matrix :: list() | tuple() | a_record() | proplists:proplist() | map() | gb_trees:tree().

verify_handler(Handler,Model,Matrix) ->
	Handler:verify(return_structure,Model,Matrix).
	

%% ----------------------------
%% @doc Return arithmetical matrix model
-spec model(Kind,Properties) ->
	list() | tuple() | a_record() | proplists:proplist() | map() | gb_trees:tree()
	when
	Kind :: list | tuple | record | proplists | map | gb_tree,
	Properties :: {Name,Length} | Length | Description,
	Name :: atom(),
	Length :: pos_integer(),
	Description :: [atom()].

model(list,Length) ->
	lists:duplicate(Length,(fun is_number/1));
model(tuple,Length) ->
	list_to_tuple(model(list,Length));
model(record,{Name,Length}) ->
	a_structure_r:model(verificator,list_to_tuple(
		lists:append([Name],lists:duplicate(Length,1))
	));
model(proplist,Description) ->
	[{Name,(fun is_number/1)} || Name <- Description];
model(map,Description) ->
	maps:from_list(model(proplist,Description));
model(gb_tree,Description) ->
	gb_trees:from_orddict(model(proplist,Description)).