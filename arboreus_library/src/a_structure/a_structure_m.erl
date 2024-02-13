%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus map based data structures handler
%%%
%%% @end
%%% Created : 06/21/2018 at 21:41
%%%-------------------------------------------------------------------
-module(a_structure_m).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types
-include("../include/types/types_a_general.hrl").

%% Data models

%% API
-export([
	test/0,
	verify/3,
	mass_verify/2,mass_verify/3,
	model/2,
	reference/1,reference/2,reference/3,
	elements/1,elements/2,
	sort/2,sorting_elements_handler/3,
	values/3,
	rotate/2
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_structure_m) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Map1 = #{a => 1,b => one,c => 0.1,d => "11"},
	Map2 = #{a => 2,b => two,c => 0.1,d => "11"},
	Map3 = #{a => 3,b => three,c => 0.1,d => "22"},
	Map_wrong = #{a => a,b => atom,c => 0.1,d => "123"},
	Model1 = #{
		a => (fun is_number/1),
		b => (fun is_atom/1),
		c => (fun is_number/1),
		d => (fun is_list/1)
	},
	true = verify(return_boolean,Model1,Map1),
	true = verify(return_boolean,Model1,Map2),
	false = verify(return_boolean,Model1,Map_wrong),
	io:format("DONE! Fun verify/3 test passed~n"),
	Model2 = model(verificator,Map1),
	Model_description = #{a => number,b => atom,c => number,d => list},
	Model_description = model(description,Map1),
	true = verify(return_boolean,Model2,Map2),
	false = verify(return_boolean,Model2,Map_wrong),
	io:format("DONE! Fun model/2 test passed~n"),
	Structures = [Map1,Map2,Map3],
	Structures_wrong = [Map1,Map2,Map_wrong],
	true = mass_verify(Model1,Structures),
	false = mass_verify(Model1,Structures_wrong),
	io:format("DONE! Fun mass_verify/2 test passed~n"),
	{true,Structures} = mass_verify(return_list,Model1,Structures),
	{true,Structures} = mass_verify(return_list,Model2,Structures),
	io:format("DONE! Fun mass_verify/3 test passed~n"),
	{true,Reference1} = reference(Structures),
	{true,Reference1} = reference(Structures,all),
	{true,Reference1} = reference(Structures,all,[]),
	{true,Reference2} = reference(Structures,[a,b,c,d],[]),
	Value_one = [1,2,3],
	Value_one = proplists:get_value(a,Reference2),
	Value_one = proplists:get_value(a,Reference1),
	Value_two = [one,two,three],
	Value_two = proplists:get_value(b,Reference2),
	Value_two = proplists:get_value(b,Reference1),
	Value_three = [0.1],
	Value_three = proplists:get_value(c,Reference2),
	Value_three = proplists:get_value(c,Reference1),
	Value_four = ["11","22"],
	Value_four = proplists:get_value(d,Reference2),
	Value_four = proplists:get_value(d,Reference1),
	io:format("DONE! Fun reference/3 test passed: ~p~n",[Reference1]),
	List_for_sorting = [
		#{a => one,b => 1,c => 0.1,d => "11"},
		#{a => two,b => 2,c => 0.1,d => "11"},
		#{a => three,b => 5,c => 0.1,d => "11"},
		#{a => four,b => 3,c => 0.1,d => "11"},
		#{a => five,b => 4,c => 0.1,d => "11"}
	],
	List_sorted = [
		#{a => one,b => 1,c => 0.1,d => "11"},
		#{a => two,b => 2,c => 0.1,d => "11"},
		#{a => four,b => 3,c => 0.1,d => "11"},
		#{a => five,b => 4,c => 0.1,d => "11"},
		#{a => three,b => 5,c => 0.1,d => "11"}
	],
	false = sort({start,List_for_sorting},[zero]),
	List_sorted = sort({start,List_for_sorting},[b]),
	List_sorted_all = [
		#{a => five,b => 4,c => 0.1,d => "11"},
		#{a => four,b => 3,c => 0.1,d => "11"},
		#{a => one,b => 1,c => 0.1,d => "11"},
		#{a => three,b => 5,c => 0.1,d => "11"},
		#{a => two,b => 2,c => 0.1,d => "11"}
	],
	List_sorted_all = sort({start,List_for_sorting},all),
	io:format("DONE! Fun sort/2 test passed: ~p~n",[List_sorted]),
	[{a,[one,two,three,four,five]},
		{b,[1,2,5,3,4]},
		{c,[0.1,0.1,0.1,0.1,0.1]}] = values(List_for_sorting,[a,b,c],plain),
	Result_plain_all = [{a,[one,two,three,four,five]},
		{b,[1,2,5,3,4]},
		{c,[0.1,0.1,0.1,0.1,0.1]},
		{d,["11","11","11","11","11"]}],
	Result_plain_all = values(List_for_sorting,all,plain),
	[{a,[{1,one},{2,two},{3,three},{4,four},{5,five}]},
		{b,[{1,1},{2,2},{3,5},{4,3},{5,4}]},
		{c,[{1,0.1},{2,0.1},{3,0.1},{4,0.1},{5,0.1}]}] = values(List_for_sorting,[a,b,c],numbered),
	Result_numbered_all = [{a,[{1,one},{2,two},{3,three},{4,four},{5,five}]},
		{b,[{1,1},{2,2},{3,5},{4,3},{5,4}]},
		{c,[{1,0.1},{2,0.1},{3,0.1},{4,0.1},{5,0.1}]},
		{d,[{1,"11"},{2,"11"},{3,"11"},{4,"11"},{5,"11"}]}],
	Result_numbered_all = values(List_for_sorting,all,numbered),
	io:format("DONE! Fun values/3 test passed~n"),
	Result_plain_all = rotate({numbered,a_list:numerate(List_for_sorting)},plain),
	Result_numbered_all = rotate({numbered,a_list:numerate(List_for_sorting)},numbered),
	Result_plain_all = rotate(List_for_sorting,plain),
	Result_numbered_all = rotate(List_for_sorting,numbered),
	io:format("DONE! Fun rotate/2 test passed~n"),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_structure_m) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Rotate structures
-spec rotate(Structures,Kind) -> proplists:proplist() | false
	when
	Structures :: {numbered,[{pos_integer(),map()}]} | list(),
	Kind :: plain | numbered.

rotate({numbered,Structures},Kind) ->
	rotate_handler(Structures,Kind,[]);
rotate(Structures,Kind) ->
	[Etalon|_] = Structures,
	case mass_verify(model(verificator,Etalon),Structures) of
		true -> rotate({numbered,a_list:numerate(Structures)},Kind);
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc Rotate structures functionality handler
-spec rotate_handler(Structures,Kind,Output) -> Output
	when
	Structures :: [{Id,a_record()}],
	Id :: pos_integer(),
	Kind :: plain | numbered,
	Output :: proplists:proplist().

rotate_handler([],_,Output) -> Output;
rotate_handler([{Id,Structure}|Structures],Kind,[]) ->
	rotate_handler(
		Structures,Kind,
		[{Position,case Kind of
			plain -> [Value];
			_ -> [{Id,Value}]
		end} || {Position,Value} <- maps:to_list(Structure)]
	);
rotate_handler([{Id,Structure}|Structures],Kind,Output) ->
	rotate_handler(
		Structures,Kind,
		[{Position,lists:append(
			Values,case Kind of
				plain -> [maps:get(Position,Structure)];
				_ -> [{Id,maps:get(Position,Structure)}]
			end
		)} || {Position,Values} <- Output]
	).


%% ----------------------------
%% @doc Return proplist within values of structures selected and grouped by positions
-spec values(Structures,Positions,Kind) -> proplists:proplist()
	when
	Structures :: a_list_of_maps(),
	Positions :: a_list_of_integers() | all,
	Kind :: plain | numbered.

values(Structures,all,Kind) ->
	[Etalon|_] = Structures,
	values(Structures,maps:keys(Etalon),Kind);
values(Structures,Positions,Kind) ->
	a_structure:values(?MODULE,Structures,Positions,Kind).


%% ----------------------------
%% @doc Sorting structures by defined list of elements
-spec sort(Structures,Positions) -> Structures | false
	when
	Structures :: a_list_of_maps(),
	Positions :: a_list_of_values() | all.

sort({start,Structures},all) ->
	[Etalon|_] = Structures,
	Model = model(verificator,Etalon),
	case mass_verify(Model,Structures) of
		true -> sort(Structures,maps:keys(Model));
		Verification_result -> Verification_result
	end;
sort({start,Structures},Positions) ->
	[Etalon|_] = Structures,
	Model = model(verificator,Etalon),
	case mass_verify(Model,Structures) of
		true ->
			Check_positions = fun
				F([]) -> true;
				F([F_position|F_positions]) ->
					Check_position = try maps:get(F_position,Model) catch _:_ -> false end,
					case Check_position of
						false -> false;
						_ -> F(F_positions)
					end
			end,
			case Check_positions(Positions) of
				true -> sort(Structures,Positions);
				_ -> false
			end;
		Verification_result -> Verification_result
	end;
sort([Structure|Structures],Positions) ->
	{Smaller,Larger} = a_structure:sort_handler(
		?MODULE,Positions,
		sorting_elements_handler(Positions,Structure,[]),
		Structures,[],[]
	),
	lists:append([sort(Smaller,Positions),[Structure],sort(Larger,Positions)]);
sort([],_) -> [].


%% ----------------------------
%% @doc Making list of elements for sorting
-spec sorting_elements_handler(Positions,Structure,Output) -> Output
	when
	Positions :: a_list_of_integers(),
	Structure :: list(),
	Output :: list().

sorting_elements_handler([],_,Output) -> Output;
sorting_elements_handler([Position|Positions],Structure,Output) ->
	sorting_elements_handler(
		Positions,Structure,lists:append(Output,[
			maps:get(Position,Structure)
		])).


%% ----------------------------
%% @doc Wrapper for reference/2
-spec reference(Structures) -> false | {true,Reference}
	when
	Structures :: list(),
	Reference :: proplists:proplist().

reference(Structures) -> reference(Structures,all).


%% ----------------------------
%% @doc Wrapper for reference/3
-spec reference(Structures,Positions) -> false | {true,Reference}
	when
	Structures :: list(),
	Positions :: list() | all,
	Reference :: proplists:proplist().

reference(Structures,Positions) -> reference(Structures,Positions,[]).


%% ----------------------------
%% @doc Generate reference
-spec reference(Structures,Positions,Reference) ->
	false | {true,Reference}
	when
	Structures :: list(),
	Positions :: list() | all,
	Reference :: proplists:proplist().

reference(Structures,all,Reference) ->
	[Etalon|_] = Structures,
	a_structure:reference(
		?MODULE,Structures,maps:keys(Etalon),Reference
	);
reference(Structures,Positions,Reference) ->
	a_structure:reference(
		?MODULE,Structures,Positions,Reference
	).


%% ----------------------------
%% @doc Wrapper for elements/2
-spec elements(Structure) -> proplists:proplist()
	when
	Structure :: map().

elements(Structure) -> elements(maps:keys(Structure),Structure,[]).


%% ----------------------------
%% @doc Wrapper for elements/3
-spec elements(Positions,Structure) -> proplists:proplist()
	when
	Positions :: a_list_of_integers(),
	Structure :: map().

elements(Positions,Structure) -> elements(Positions,Structure,[]).


%% ----------------------------
%% @doc Return proplist within position-value pair of the structure
-spec elements(Positions,Structure,Elements) -> proplists:proplist()
	when
	Positions :: list(),
	Structure :: map(),
	Elements :: proplists:proplist().

elements([],_,Elements) -> Elements;
elements([Position|Positions],Structure,Elements) ->
	elements(
		Positions,Structure,
		lists:append(Elements,[
			{Position,maps:get(Position,Structure)}
		])
	).


%% ----------------------------
%% @doc Return data model of the structure
-spec model(Kind,Structure) -> tuple()
	when
	Kind :: verificator | description,
	Structure :: map().

model(Kind,Structure) ->
	model_handler(Kind,#{},maps:iterator(Structure)).


%% ----------------------------
%% @doc Handler for map/2
-spec model_handler(Kind,Model,Structure) -> map()
	when
	Kind :: verificator | description,
	Model :: map(),
	Structure :: map().

model_handler(_,Model,none) -> Model;
model_handler(Kind,Model,Structure) ->
	{Name,Element,Structure_next} = maps:next(Structure),
	model_handler(
		Kind,
		maps:put(Name,a_var:inspector(Kind,Element),Model),
		Structure_next
	).


%% ----------------------------
%% @doc The structures massive verification
-spec mass_verify(Model,List_of_structures) -> boolean()
	when
	Model :: map(),
	List_of_structures :: a_list_of_maps().

mass_verify(Model,List_of_structures) ->
	mass_verify_handler(Model,List_of_structures).


%% ----------------------------
%% @doc The structures massive verification, adjusted return
-spec mass_verify(Return_mode,Model,List_of_structures) ->
	{true,List_of_structures} | boolean()
	when
	Return_mode :: return_list | return_boolean,
	Model :: map(),
	List_of_structures :: a_list_of_maps().

mass_verify(return_list,Model,List_of_structures) ->
	case mass_verify_handler(Model,List_of_structures) of
		true -> {true,List_of_structures};
		Verification_result -> Verification_result
	end;
mass_verify(_,Model,List_of_structures) ->
	mass_verify_handler(Model,List_of_structures).


%% ----------------------------
%% @doc The structures massive verification handler
-spec mass_verify_handler(Model,List_of_structures) -> boolean()
	when
	Model :: map(),
	List_of_structures :: a_list_of_maps().

mass_verify_handler(_,[]) -> true;
mass_verify_handler(Model,[Structure|List_of_structures]) ->
	case verify(return_boolean,Model,Structure) of
		true -> mass_verify_handler(Model,List_of_structures);
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc Structure verification
-spec verify(Return_mode,Model,Structure) -> boolean() | {true,Structure}
	when
	Return_mode :: return_structure | return_boolean,
	Model :: map(),
	Structure :: map().

verify(Return_mode,Model,Structure) ->
	if
		map_size(Structure) == map_size(Model) ->
			try
				case Return_mode of
					return_structure -> verify_structure(maps:iterator(Model),Structure);
					_ -> verify_boolean(maps:iterator(Model),Structure)
				end
			catch _:_ -> false end;
		true -> false
	end.


%% ----------------------------
%% @doc Structure verification handler, data return mode
-spec verify_structure(Model,Structure) -> {true,Structure} | false
	when
	Model :: map(),
	Structure :: map().

verify_structure(Model,Structure) ->
	case verify_boolean(Model,Structure) of
		true -> {true,Structure};
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc Structure verification handler, boolean return mode
-spec verify_boolean(Model,Structure) -> boolean()
	when
	Model :: map(),
	Structure :: map().

verify_boolean(none,_) -> true;
verify_boolean(Model,Structure) ->
	{Name,Inspector,Next_model} = maps:next(Model),
	case Inspector(maps:get(Name,Structure)) of
		true -> verify_boolean(Next_model,Structure);
		_ -> false
	end.