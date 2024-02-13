%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus structures library
%%%
%%% @end
%%% Created : 07/02/2018 at 14:47
%%%-------------------------------------------------------------------
-module(a_structure).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types
-include("../include/types/types_a_general.hrl").

%% Data models

%% API
-export([
	test/0,
	reference/2,reference/3,reference/4,
	sort_handler/6,
	values/4
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Return proplist within values of structures selected and grouped by positions
-spec values(Module,Structures,Positions,Kind) -> proplists:proplist()
	when
	Module :: module(),
	Structures :: {numbered,a_list_numerated()} | list(),
	Positions :: a_list_of_values(),
	Kind :: plain | numbered.

values(Module,{numbered,Structures},Positions,Kind) ->
	Elements = fun
		F2([],_,F2_output) -> F2_output;
		F2([{F2_count,F2_structure}|F2_structures],F2_positions,F2_output) ->
			F2(
				F2_structures,F2_positions,
				lists:append(F2_output,[{F2_count,Module:elements(Positions,F2_structure)}])
			)
	end,
	values_handler(Kind,Elements(Structures,Positions,[]),[]);
values(Module,Structures,Positions,Kind) ->
	[Etalon|_] = Structures,
	case Module:mass_verify(Module:model(verificator,Etalon),Structures) of
		true ->
			Elements = fun
				F2([],_,F2_output) -> F2_output;
				F2([{F2_count,F2_structure}|F2_structures],F2_positions,F2_output) ->
					F2(
						F2_structures,F2_positions,
						lists:append(F2_output,[{F2_count,Module:elements(Positions,F2_structure)}])
					)
			end,
			values_handler(Kind,Elements(a_list:numerate(Structures),Positions,[]),[]);
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc The values procedure handler
-spec values_handler(Kind,Structures,Output) -> Output
	when
	Kind :: plain | numbered,
	Structures :: list(),
	Output :: proplists:proplist().

values_handler(_,[],Output) -> Output;
values_handler(Kind,[{Structure_id,Elements}|Structures],[]) ->
	values_handler(
		Kind,Structures,
		case Kind of
			plain -> [{Position,[Value]} || {Position,Value} <- Elements];
			_ -> [{Position,[{Structure_id,Value}]} || {Position,Value} <- Elements]
		end
	);
values_handler(Kind,[{Structure_id,Elements}|Structures],Output) ->
	Generate_output = fun
		F([],F_output) -> F_output;
		F([{F_position,F_value}|F_elements],F_output) ->
			F(
				F_elements,
				lists:keyreplace(
					F_position,1,F_output,
					{F_position,lists:append(
						proplists:get_value(F_position,F_output),
						case Kind of
							plain -> [F_value];
							_ -> [{Structure_id,F_value}]
						end
				)})
			)
	end,
	values_handler(Kind,Structures,Generate_output(Elements,Output)).


%% ----------------------------
%% @doc Sorting structures procedure handler
-spec sort_handler(Module,Positions,Check,Structures,Smaller,Larger) -> list()
	when
	Module :: module(),
	Positions :: a_list_of_integers(),
	Check :: a_list_of_values(),
	Structures :: list() | proplists:proplist() | a_record() | tuple() | map() | gb_trees:tree(),
	Smaller :: list(),
	Larger :: list().

sort_handler(Module,Positions,Check,[Structure|Structures],Smaller,Larger) ->
	Structure_check = Module:sorting_elements_handler(Positions,Structure,[]),
	case Structure_check =< Check of
		true ->
			sort_handler(
				Module,Positions,Check,Structures,
				[Structure|Smaller],Larger
			);
		false ->
			sort_handler(
				Module,Positions,Check,Structures,
				Smaller,[Structure|Larger]
			)
	end;
sort_handler(_,_,_,[],Smaller,Larger) -> {Smaller,Larger}.


%% ----------------------------
%% @doc Wrapper for reference/2
-spec reference(Module,Structures) -> false | {true,Reference}
	when
	Module :: a_structure_r | a_structure_l | a_structure_t | a_structure_m |
	a_structure_gb | a_structure_pl,
	Structures :: list(),
	Reference :: proplists:proplist().

reference(Module,Structures) -> reference(Module,Structures,all).


%% ----------------------------
%% @doc Wrapper for reference/3
-spec reference(Module,Structures,Positions) -> false | {true,Reference}
	when
	Module :: a_structure_r | a_structure_l | a_structure_t | a_structure_m |
	a_structure_gb | a_structure_pl,
	Structures :: list(),
	Positions :: list() | all,
	Reference :: proplists:proplist().

reference(Module,Structures,Positions) ->
	reference(Module,Structures,Positions,[]).


%% ----------------------------
%% @doc Generate reference
-spec reference(Module,Structures,Positions,Reference) ->
	false | {true,Reference}
	when
	Module :: a_structure_r | a_structure_l | a_structure_t | a_structure_m |
	a_structure_gb | a_structure_pl,
	Structures :: list(),
	Positions :: list() | {all,Length},
	Length :: pos_integer(),
	Reference :: proplists:proplist().

reference(Module,Structures,{all,Length},Reference) ->
	[Etalon|_] = Structures,
	reference_handler(
		Module,Structures,Module:model(verificator,Etalon),
		lists:seq(1,Length),Reference
	);
reference(Module,Structures,Positions,Reference) ->
	[Etalon|_] = Structures,
	reference_handler(
		Module,Structures,Module:model(verificator,Etalon),
		Positions,Reference
	).


%% ----------------------------
%% @doc Generating reference procedure handler
-spec reference_handler(Module,Structures,Model,Positions,Reference) ->
	false | {true,Reference}
	when
	Module :: a_structure_r | a_structure_l | a_structure_t | a_structure_m |
		a_structure_gb | a_structure_pl,
	Structures :: list(),
	Model :: list(),
	Positions :: a_list_of_integers(),
	Reference :: proplists:proplist().

reference_handler(_,[],_,_,Reference) -> {true,Reference};
reference_handler(Module,[Structure|Structures],Model,Positions,Reference) ->
	case Module:verify(return_boolean,Model,Structure) of
		true ->
			Compose_reference = fun
				Function([],Reference_income) -> Reference_income;
				Function([{Key,Value}|List],Reference_income) ->
					case proplists:get_value(Key,Reference_income) of
						undefined ->
							Function(List,lists:append(Reference_income,[{Key,[Value]}]));
						Reference_value ->
							Function(List,lists:keyreplace(
								Key,1,Reference_income,{Key,begin
									case lists:member(Value,Reference_value) of
										false -> lists:append(Reference_value,[Value]);
										_ -> Reference_value
									end
								end
								}))
					end
			end,
			reference_handler(
				Module,Structures,Model,Positions,
				Compose_reference(Module:elements(Positions,Structure),Reference)
			);
		Verification_result -> Verification_result
	end.