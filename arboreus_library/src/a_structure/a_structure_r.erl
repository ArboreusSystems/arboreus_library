%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus record based data structures handler
%%%
%%% @end
%%% Created : 06/21/2018 at 21:40
%%%-------------------------------------------------------------------
-module(a_structure_r).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types
-include("../include/types/types_general.hrl").

%% Data models
-record(test,{one,two,three,four}).
-record(test1,{one,two,three,four}).

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
		"Module (a_structure_r) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Record1 = #test{one = 1,two = one,three = 0.1,four = "11"},
	Record2 = #test{one = 2,two = two,three = 0.1,four = "11"},
	Record3 = #test{one = 3,two = three,three = 0.1,four = "22"},
	Record_wrong1 = #test{one = one,two = second_atom,three = 0.2,four = "1234"},
	Record_wrong2 = #test1{one = two,two = second_atom,three = 0.2,four = "1234"},
	Model1 = model(verificator,Record1),
	{name,number,atom,number,list} = model(description,Record1),
	true = verify(return_boolean,Model1,Record1),
	true = verify(return_boolean,Model1,Record2),
	false = verify(return_boolean,Model1,Record_wrong1),
	false = verify(return_boolean,Model1,Record_wrong2),
	io:format("DONE! Fun verify/3 test passed~n"),
	io:format("DONE! Fun model/2 test passed~n"),
	Structures = [Record1,Record2,Record3],
	Structures_wrong = [Record1,Record2,Record_wrong1],
	true = mass_verify(Model1,Structures),
	false = mass_verify(Model1,Structures_wrong),
	false = mass_verify(Model1,[]),
	true = mass_verify([],[]),
	false = mass_verify([],Structures),
	io:format("DONE! Fun mass_verify/2 test passed~n"),
	{true,Structures} = mass_verify(return_list,Model1,Structures),
	io:format("DONE! Fun mass_verify/3 test passed~n"),
	{true,Reference1} = reference(Structures),
	{true,Reference1} = reference(Structures,all),
	{true,Reference1} = reference(Structures,all,[]),
	{true,Reference1} = reference(Structures,[1,2,3,4],[]),
	io:format("DONE! Fun reference/3 test passed: ~p~n",[Reference1]),
	List_for_sorting = [
		#test{one = 1,two = one},
		#test{one = 2,two = two},
		#test{one = 5,two = three},
		#test{one = 3,two = four},
		#test{one = 4,two = five}
	],
	List_sorted = [
		{test,1,one,undefined,undefined},
		{test,2,two,undefined,undefined},
		{test,3,four,undefined,undefined},
		{test,4,five,undefined,undefined},
		{test,5,three,undefined,undefined}
	],
	List_sorted = sort({start,List_for_sorting},[1]),
	List_sorted = sort({start,List_for_sorting},all),
	io:format("DONE! Fun sort/2 test passed: ~p~n",[List_sorted]),
	[{1,[1,2,5,3,4]},
		{2,[one,two,three,four,five]},
		{3,[undefined,undefined,undefined,undefined,undefined]}] = values(List_for_sorting,[1,2,3],plain),
	Result_all_plain = [{1,[1,2,5,3,4]},
		{2,[one,two,three,four,five]},
		{3,[undefined,undefined,undefined,undefined,undefined]},
		{4,[undefined,undefined,undefined,undefined,undefined]}],
	Result_all_plain = values(List_for_sorting,all,plain),
	[{1,[{1,1},{2,2},{3,5},{4,3},{5,4}]},
		{2,[{1,one},{2,two},{3,three},{4,four},{5,five}]},
		{3,
			[{1,undefined},
				{2,undefined},
				{3,undefined},
				{4,undefined},
				{5,undefined}]}] = values(List_for_sorting,[1,2,3],numbered),
	Result_all_numbered = [{1,[{1,1},{2,2},{3,5},{4,3},{5,4}]},
		{2,[{1,one},{2,two},{3,three},{4,four},{5,five}]},
		{3,
			[{1,undefined},
				{2,undefined},
				{3,undefined},
				{4,undefined},
				{5,undefined}]},
		{4,
			[{1,undefined},
				{2,undefined},
				{3,undefined},
				{4,undefined},
				{5,undefined}]}],
	Result_all_numbered = values(List_for_sorting,all,numbered),
	io:format("DONE! Fun values/3 test passed~n"),
	Result_all_plain = rotate({numbered,a_list:numerate(List_for_sorting)},plain),
	Result_all_numbered = rotate({numbered,a_list:numerate(List_for_sorting)},numbered),
	Result_all_plain = rotate(List_for_sorting,plain),
	Result_all_numbered = rotate(List_for_sorting,numbered),
	io:format("DONE! Fun rotate/2 test passed~n"),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_structure_r) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Rotate structures
-spec rotate(Structures,Kind) -> proplists:proplist() | false
	when
	Structures :: {numbered,[{pos_integer(),tuple()}]} | list(),
	Kind :: plain | numbered.

rotate({numbered,Structures},Kind) ->
	[{Position - 1,Values} || {Position,Values} <- rotate_handler(Structures,Kind,[])];
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
	[_|Output] = a_list:numerate(case Kind of
		plain -> [[Element] || Element <- tuple_to_list(Structure)];
		_ -> [[{Id,Element}] || Element <- tuple_to_list(Structure)]
	end),
	rotate_handler(Structures,Kind,Output);
rotate_handler([{Id,Structure}|Structures],Kind,Output) ->
	Generate_output_p2 = fun
		F(_,F_counter,F_length,F_output) when F_counter == F_length + 1 -> F_output;
		F(F_structure,F_counter,F_length,F_output) ->
			F(F_structure,F_counter + 1,F_length,lists:keyreplace(
				F_counter,1,F_output,{F_counter,lists:append(
					proplists:get_value(F_counter,F_output),
					case Kind of
						plain -> [element(F_counter,F_structure)];
						_ -> [{Id,element(F_counter,F_structure)}]
					end
				)}
			))
	end,
	rotate_handler(
		Structures,Kind,
		Generate_output_p2(Structure,2,tuple_size(Structure),Output)
	).


%% ----------------------------
%% @doc Return proplist within values of structures selected and grouped by positions
-spec values(Structures,Positions,Kind) -> proplists:proplist()
	when
	Structures :: a_list_of_records(),
	Positions :: a_list_of_integers() | all,
	Kind :: plain | numbered.

values(Structures,all,Kind) ->
	[Etalon|_] = Structures,
	values(Structures,lists:seq(1,tuple_size(Etalon) - 1),Kind);
values(Structures,Positions,Kind) ->
	[{Position - 1, Values} || {Position,Values} <- a_structure:values(
		?MODULE,Structures,
		[Position + 1 || Position <- Positions],Kind
	)].


%% ----------------------------
%% @doc Sorting structures by defined list of elements
-spec sort(Structures,Positions) -> Structures | false
	when
	Structures :: a_list_of_records(),
	Positions :: a_list_of_integers().

sort({start,Structures},all) ->
	[Etalon|_] = Structures,
	case mass_verify(model(verificator,Etalon),Structures) of
		true -> sort(Structures,lists:seq(1,tuple_size(Etalon) - 1));
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
	Structure :: a_record(),
	Output :: list().

sorting_elements_handler([],_,Output) -> Output;
sorting_elements_handler([Position|Positions],Structure,Output) ->
	sorting_elements_handler(
		Positions,Structure,lists:append(Output,[element(Position + 1,Structure)])
	).


%% ----------------------------
%% @doc Wrapper for reference/2
-spec reference(Structures) -> false | {true,Reference}
	when
	Structures :: list(),
	Reference :: proplists:proplist().

reference(Structures) ->
	case reference_handler(Structures) of
		{true,Reference} ->
			{true,[{Position - 1,Values} || {Position,Values} <- Reference]};
		Result -> Result
	end.


%% ----------------------------
%% @doc Wrapper for reference/3
-spec reference(Structures,Positions) -> false | {true,Reference}
	when
	Structures :: list(),
	Positions :: list() | all,
	Reference :: proplists:proplist().

reference(Structures,Positions) ->
	case reference_handler(Structures,Positions,[]) of
		{true,Reference} ->
			{true,[{Position - 1,Values} || {Position,Values} <- Reference]};
		Result -> Result
	end.


%% ----------------------------
%% @doc Generate reference
-spec reference(Structures,Positions,Reference) ->
	false | {true,Reference}
	when
	Structures :: list(),
	Positions :: list() | all,
	Reference :: proplists:proplist().

reference(Structures,Positions,Reference) ->
	case reference_handler(Structures,Positions,Reference) of
		{true,Reference_out} ->
			{true,[{Position - 1,Values} || {Position,Values} <- Reference_out]};
		Result -> Result
	end.


%% ----------------------------
%% @doc Wrapper for reference_handler/2
-spec reference_handler(Structures) -> false | {true,Reference}
	when
	Structures :: list(),
	Reference :: proplists:proplist().

reference_handler(Structures) -> reference_handler(Structures,all).


%% ----------------------------
%% @doc Wrapper for reference_handler/3
-spec reference_handler(Structures,Positions) -> false | {true,Reference}
	when
	Structures :: list(),
	Positions :: list() | all,
	Reference :: proplists:proplist().

reference_handler(Structures,Positions) ->
	reference_handler(Structures,Positions,[]).


%% ----------------------------
%% @doc Generate reference
-spec reference_handler(Structures,Positions,Reference) ->
	false | {true,Reference}
	when
	Structures :: list(),
	Positions :: list() | all,
	Reference :: proplists:proplist().

reference_handler(Structures,all,Reference) ->
	[Etalon|_] = Structures,
	a_structure:reference(
		?MODULE,Structures,lists:seq(2,tuple_size(Etalon)),Reference
	);
reference_handler(Structures,Positions,Reference) ->
	a_structure:reference(
		?MODULE,Structures,
		[Position + 1 || Position <- Positions],Reference
	).


%% ----------------------------
%% @doc Wrapper for elements/3
-spec elements(Structure) -> proplists:proplist()
	when
	Structure :: list().

elements(Structure) ->
	a_structure_t:elements(lists:seq(2,tuple_size(Structure)),Structure).


%% ----------------------------
%% @doc Wrapper for elements/3
-spec elements(Positions,Structure) -> proplists:proplist()
	when
	Positions :: a_list_of_integers(),
	Structure :: list().

elements(Positions,Structure) ->
	a_structure_t:elements(Positions,Structure).


%% ----------------------------
%% @doc Return data model of the structure
-spec model(Kind,Structure) -> tuple()
	when
	Kind :: verificator | description,
	Structure :: a_record().

model(Kind,Structure) ->
	if
		tuple_size(Structure) >= 2 ->
			Record_name = element(1,Structure),
			Name_inspector = case Kind of
				description -> name;
				_ ->
					(fun(Name) ->
						case is_atom(Name) of
							true ->
								if
									Record_name == Name -> true;
									true -> false
								end;
							Result -> Result
						end
					end)
			end,
			[_|Structure_data] = tuple_to_list(Structure),
			list_to_tuple(lists:append(
				[Name_inspector],
				a_structure_l:model(Kind,Structure_data)
			));
		true -> false
	end.


%% ----------------------------
%% @doc The structures massive verification
-spec mass_verify(Model,List_of_structures) -> boolean()
	when
	Model :: tuple(),
	List_of_structures :: a_list_of_records().

mass_verify(Model,List_of_structures) ->
	a_structure_t:mass_verify(Model,List_of_structures).


%% ----------------------------
%% @doc The structures massive verification, adjusted return
-spec mass_verify(Return_mode,Model,List_of_structures) ->
	{true,List_of_structures} | boolean()
	when
	Return_mode :: return_list | return_boolean,
	Model :: tuple(),
	List_of_structures :: a_list_of_records().

mass_verify(Return_mode,Model,List_of_structures) ->
	a_structure_t:mass_verify(Return_mode,Model,List_of_structures).


%% ----------------------------
%% @doc List structure verification
-spec verify(Return_mode,Model,Structure) -> boolean() | {true,Structure}
	when
	Return_mode :: return_structure | return_boolean,
	Model :: tuple(),
	Structure :: a_record().

verify(Return_mode,Model,Structure) ->
	a_structure_t:verify(Return_mode,Model,Structure).