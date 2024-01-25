%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus list based data structures handler
%%%
%%% @end
%%% Created : 06/21/2018 at 10:27
%%%-------------------------------------------------------------------
-module(a_structure_l).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types
-include("../include/types/types_general.hrl").

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
	rotate/2,
	plain_rotate/2
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_structure_l) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	List1 = [1,atom,0.1,"11"],
	List2 = [2,second_atom,0.1,"11"],
	List3 = [3,third_atom,0.1,"22"],
	List_wrong = [one,atom,0.1,"123"],
	Model1 = [(fun is_integer/1),(fun is_atom/1),(fun is_float/1),(fun is_list/1)],
	true = verify(return_boolean,Model1,List1),
	true = verify(return_boolean,Model1,List2),
	{true,List1} = verify(return_structure,Model1,List1),
	false = verify(return_boolean,Model1,List_wrong),
	false = verify(return_boolean,[],List1),
	io:format("DONE! Fun verify/3 test passed~n"),
	Model2 = model(verificator,List1),
	false = verify(return_boolean,Model2,List_wrong),
	true = verify(return_boolean,Model2,List2),
	io:format("DONE! Fun model/2 test passed~n"),
	Structures = [List1,List2,List3],
	Structures_wrong = [List1,List2,List_wrong],
	true = mass_verify(Model1,Structures),
	true = mass_verify(Model2,Structures),
	false = mass_verify(Model1,Structures_wrong),
	false = mass_verify(Model2,Structures_wrong),
	false = mass_verify(Model1,[]),
	true = mass_verify([],[]),
	false = mass_verify([],Structures),
	io:format("DONE! Fun mass_verify/2 test passed~n"),
	{true,Structures} = mass_verify(return_list,Model1,Structures),
	{true,Structures} = mass_verify(return_list,Model2,Structures),
	io:format("DONE! Fun mass_verify/3 test passed~n"),
	{true,Reference1} = reference(Structures),
	{true,Reference1} = reference(Structures,all),
	{true,Reference1} = reference(Structures,all,[]),
	{true,Reference1} = reference(Structures,[1,2,3,4],[]),
	io:format("DONE! Fun reference/3 test passed: ~p~n",[Reference1]),
	List_for_sorting = [
		[one,1,2,3,7,4],[two,2,8,1,1,6],
		[three,5,1,5,7,4],[four,3,2,3,7,4],
		[five,4,2,3,7,4]
	],
	List_sorted = [
		[one,1,2,3,7,4],[two,2,8,1,1,6],
		[four,3,2,3,7,4],[five,4,2,3,7,4],
		[three,5,1,5,7,4]],
	List_sorted_all = [
		[five,4,2,3,7,4],[four,3,2,3,7,4],
		[one,1,2,3,7,4],[three,5,1,5,7,4],
		[two,2,8,1,1,6]
	],
	List_sorted = sort({start,List_for_sorting},[2]),
	List_sorted_all = sort({start,List_for_sorting},all),
	io:format("DONE! Fun sort/2 test passed: ~p~n",[List_sorted]),
	List_values_plain = [
		{1,[one,two,three,four,five]},
		{2,[1,2,5,3,4]},
		{3,[2,8,1,2,2]}
	],
	List_values_numbered = [
		{1,[{1,one},{2,two},{3,three},{4,four},{5,five}]},
		{2,[{1,1},{2,2},{3,5},{4,3},{5,4}]},
		{3,[{1,2},{2,8},{3,1},{4,2},{5,2}]}
	],
	List_values_plain = values(List_for_sorting,[1,2,3],plain),
	Result_all_plain = [{1,[one,two,three,four,five]},
		{2,[1,2,5,3,4]},
		{3,[2,8,1,2,2]},
		{4,[3,1,5,3,3]},
		{5,[7,1,7,7,7]},
		{6,[4,6,4,4,4]}],
	Result_all_plain = values(List_for_sorting,all,plain),
	List_values_numbered = values(List_for_sorting,[1,2,3],numbered),
	Result_all_numbered = [{1,[{1,one},{2,two},{3,three},{4,four},{5,five}]},
		{2,[{1,1},{2,2},{3,5},{4,3},{5,4}]},
		{3,[{1,2},{2,8},{3,1},{4,2},{5,2}]},
		{4,[{1,3},{2,1},{3,5},{4,3},{5,3}]},
		{5,[{1,7},{2,1},{3,7},{4,7},{5,7}]},
		{6,[{1,4},{2,6},{3,4},{4,4},{5,4}]}],
	Result_all_numbered = values(List_for_sorting,all,numbered),
	io:format("DONE! Fun values/3 test passed~n"),
	Result_all_plain = rotate({numbered,a_list:numerate(List_for_sorting)},plain),
	Result_all_numbered = rotate({numbered,a_list:numerate(List_for_sorting)},numbered),
	Result_all_plain = rotate(List_for_sorting,plain),
	Result_all_numbered = rotate(List_for_sorting,numbered),
	io:format("DONE! Fun rotate/2 test passed~n"),
	Structures_plain = [[1,1,1,1,1],[2,2,2,2,2],[3,3,3,3,3],[4,4,4,4,4],[5,5,5,5,5]],
	Structures_plain = a_structure_l:plain_rotate(
		cw,a_structure_l:plain_rotate(ccw,Structures_plain)
	),
	io:format("DONE! Fun plain_rotate/2 test passed~n"),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_structure_l) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Rotate the list based structure
-spec plain_rotate(Kind,Structures) -> a_list_of_lists()
	when
	Kind :: ccw | cw,
	Structures :: a_list_of_lists().

plain_rotate(ccw,Structures) ->
	plain_rotate_handler(Structures,[]);
plain_rotate(cw,Structures) ->
	plain_rotate_handler(lists:reverse(Structures),[]).


%% ----------------------------
%% @doc The plain rotate procedure handler
-spec plain_rotate_handler(Structures,Output) -> a_list_of_lists()
	when
	Structures :: a_list_of_lists(),
	Output :: a_list_of_lists().

plain_rotate_handler([],Output) -> Output;
plain_rotate_handler([Structure|Structures],[]) ->
	plain_rotate_handler(
		Structures,[[Element] || Element <- Structure]
	);
plain_rotate_handler([Structure|Structures],Output) ->
	Create_output = fun
		F([],[],F_output) -> F_output;
		F([F_values|F_output_in],[F_value|F_structure],F_output) ->
			F(
				F_output_in,F_structure,
				lists:append(F_output,[lists:append(F_values,[F_value])])
			)
	end,
	plain_rotate_handler(
		Structures,Create_output(Output,Structure,[])
	).


%% ----------------------------
%% @doc Rotate structures
-spec rotate(Structures,Kind) -> proplists:proplist() | false
	when
	Structures :: {numbered,[{pos_integer(),any()}]} | list(),
	Kind :: plain | numbered.

rotate({numbered,Structures},Kind) ->
	rotate_handler(Structures,Kind,[]);
rotate(Structures,Kind) ->
	[Etalon|_] = Structures,
	case mass_verify(return_boolean,model(verificator,Etalon),Structures) of
		true -> rotate({numbered,a_list:numerate(Structures)},Kind);
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc Rotate structures functionality handler
-spec rotate_handler(Structures,Kind,Output) -> Output
	when
	Structures :: [{Id,Structure}],
	Id :: pos_integer(),
	Structure :: list(),
	Kind :: plain | numbered,
	Output :: proplists:proplist().

rotate_handler([],_,Output) -> Output;
rotate_handler([{Id,Structure}|Structures],Kind,[]) ->
	Generate_output_p1 = fun
		F([],_,_,_,F_output) -> F_output;
		F([F_element|F_elements],F_kind,F_id,F_counter,F_output) ->
			F(F_elements,F_kind,F_id,F_counter + 1,lists:append(
				F_output,[{F_counter,[case F_kind of
					plain -> F_element;
					_ -> {F_id,F_element}
				end]}]
			))
	end,
	rotate_handler(Structures,Kind,Generate_output_p1(Structure,Kind,Id,1,[]));
rotate_handler([{Id,Structure}|Structures],Kind,Output) ->
	Generate_output_p2 = fun
		F([],_,_,_,F_output) -> F_output;
		F([F_element|F_elements],F_kind,F_id,F_counter,F_output) ->
			F(F_elements,F_kind,F_id,F_counter + 1,lists:keyreplace(
				F_counter,1,F_output,{F_counter,lists:append(
					proplists:get_value(F_counter,F_output),case F_kind of
						plain -> [F_element];
						_ -> [{F_id,F_element}]
					end)}
			))
	end,
	rotate_handler(
		Structures,Kind,
		Generate_output_p2(Structure,Kind,Id,1,Output)
	).


%% ----------------------------
%% @doc Return proplist within values of structures selected and grouped by positions
-spec values(Structures,Positions,Kind) -> proplists:proplist()
	when
	Structures :: {numerated,a_list_numerated()} | list(),
	Positions :: a_list_of_integers() | all,
	Kind :: plain | numbered.

values(Structures,all,Kind) ->
	[Etalon|_] = Structures,
	values(Structures,lists:seq(1,length(Etalon)),Kind);
values(Structures,Positions,Kind) ->
	a_structure:values(?MODULE,Structures,Positions,Kind).


%% ----------------------------
%% @doc Sorting structures by defined list of elements
-spec sort(Structures,Positions) -> Structures | false
	when
	Structures :: a_list_of_lists(),
	Positions :: a_list_of_integers() | all.

sort({start,Structures},all) ->
	[Etalon|_] = Structures,
	case mass_verify(model(verificator,Etalon),Structures) of
		true -> sort(Structures,lists:seq(1,length(Etalon)));
		Verification_result -> Verification_result
	end;
sort({start,Structures},Positions) ->
	[Etalon|_] = Structures,
	case mass_verify(model(verificator,Etalon),Structures) of
		true -> sort(Structures,Positions);
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
		Positions,Structure,lists:append(Output,[lists:nth(Position,Structure)])
	).


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
		?MODULE,Structures,{all,length(Etalon)},Reference
	);
reference(Structures,Positions,Reference) ->
	a_structure:reference(
		?MODULE,Structures,Positions,Reference
	).


%% ----------------------------
%% @doc Wrapper for elements/2
-spec elements(Structure) -> proplists:proplist()
	when
	Structure :: list().

elements(Structure) ->
	elements(lists:seq(1,length(Structure)),Structure,[]).


%% ----------------------------
%% @doc Wrapper for elements/3
-spec elements(Positions,Structure) -> proplists:proplist()
	when
	Positions :: a_list_of_integers(),
	Structure :: list().

elements(Positions,Structure) -> elements(Positions,Structure,[]).


%% ----------------------------
%% @doc Return proplist within position-value pair of the structure
-spec elements(Positions,Structure,Elements) -> proplists:proplist()
	when
	Positions :: a_list_of_integers(),
	Structure :: list(),
	Elements :: proplists:proplist().

elements([],_,Elements) -> Elements;
elements([Position|Positions],Structure,Elements) ->
	elements(
		Positions,Structure,
		lists:append(Elements,[
			{Position,lists:nth(Position,Structure)}
		])
	).


%% ----------------------------
%% @doc Return data model of the structure
-spec model(Kind,Structure) -> a_list_of_functions() | a_list_of_atoms()
	when
	Kind :: verificator | description,
	Structure :: a_list_of_values().

model(Kind,Structure) ->
	[a_var:inspector(Kind,Element) || Element <- Structure].


%% ----------------------------
%% @doc The structures massive verification
-spec mass_verify(List_of_structures,Model) -> boolean()
	when
	Model :: a_list_of_functions(),
	List_of_structures :: a_list_of_lists().

mass_verify([],[]) -> true;
mass_verify(_,[]) -> false;
mass_verify([],_) -> false;
mass_verify(Model,List_of_structures) ->
	mass_verify_handler(Model,List_of_structures).


%% ----------------------------
%% @doc The structures massive verification, adjusted return
-spec mass_verify(Return_mode,Model,List_of_structures) ->
	{true,List_of_structures} | boolean()
	when
	Return_mode :: return_list | return_boolean,
	Model :: a_list_of_functions(),
	List_of_structures :: a_list_of_lists().

mass_verify(_,[],[]) -> true;
mass_verify(_,[],_) -> false;
mass_verify(_,_,[]) -> false;
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
	Model :: a_list_of_functions(),
	List_of_structures :: a_list_of_lists().

mass_verify_handler(_,[]) -> true;
mass_verify_handler(Model,[Structure|List_of_structures]) ->
	case verify(return_boolean,Model,Structure) of
		true -> mass_verify_handler(Model,List_of_structures);
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc List structure verification
-spec verify(Return_mode,Model,Structure) -> boolean() | {true,Structure}
	when
	Return_mode :: return_structure | return_boolean,
	Model :: a_list_of_functions(),
	Structure :: a_list_of_values().

verify(_,[],[]) -> true;
verify(_,[],_) -> false;
verify(_,_,[]) -> false;
verify(Return_mode,Model,Structure) ->
	if
		length(Structure) == length(Model) ->
			case Return_mode of
				return_structure -> verify_structure(Model,Structure);
				_ -> verify_boolean(Model,Structure)
			end;
		true -> false
	end.


%% ----------------------------
%% @doc Structure verification handler, data return mode
-spec verify_structure(Model,Structure) -> {true,Structure} | false
	when
	Model :: a_list_of_functions(),
	Structure :: a_list_of_values().

verify_structure(Model,Structure) ->
	case verify_boolean(Model,Structure) of
		true -> {true,Structure};
		Inspection_result -> Inspection_result
	end.


%% ----------------------------
%% @doc Structure verification handler, boolean return mode
-spec verify_boolean(Model,Structure) -> boolean()
	when
	Model :: a_list_of_functions(),
	Structure :: a_list_of_values().

verify_boolean([],[]) -> true;
verify_boolean([Inspector|Model],[Element|Structure]) ->
	case Inspector(Element) of
		true -> verify_boolean(Model,Structure);
		Inspection_result -> Inspection_result
	end;
verify_boolean(_,_) -> false.
