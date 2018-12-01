%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV (http://alexandr.kirilov.me)
%%% @copyright (C) 2015, Arboreus, (http://arboreus.systems)
%%% @doc Arboreus lists handler
%%%
%%% @end
%%% Created : 21. Jul 2015 21:55
%%%-------------------------------------------------------------------
-module(a_list).
-author("Alexandr KIRILOV (http://alexandr.kirilov.me)").

%% Constants

%% Data types
-include("../../data_models/types/types_general.hrl").

%% Data models

%% API
-export([
	test/0,
	get_out/3,
	check/2,check/3,
	clear_duplicates/1,
	find_members/2,
	compare_members/2,
	exclude/2,
	numerate/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Numerate the list
-spec numerate(List) -> list()
	when
	List :: list().

numerate(List) ->
	numerate_handler(List,1,[]).


%% ----------------------------
%% @doc Numerate procedure handler
-spec numerate_handler(List,Count,Output) -> Output
	when
	List :: list(),
	Count :: pos_integer(),
	Output :: list().

numerate_handler([],_,Output) -> Output;
numerate_handler([Value|List],Count,Output) ->
	numerate_handler(
		List,Count + 1,
		lists:append(Output,[{Count,Value}])
	).


%% ----------------------------
%% @doc Exclude members of list from another lists and return diff
-spec exclude(List,Members) -> list()
	when
	List :: list(),
	Members :: list().

exclude(List,Members) ->
	exclude_handler(List,Members,[]).


%% ----------------------------
%% @doc Aux function for exclude/2
-spec exclude_handler(List,Members,Output) -> list()
	when
	List :: list(),
	Members :: list(),
	Output :: list().

exclude_handler([],_,Output) -> Output;
exclude_handler([Element|List],Members,Output) ->
	exclude_handler(
		List,Members,
		case lists:member(Element,Members) of
			false -> lists:append(Output,[Element]);
			_ -> Output
		end
	).


%% ----------------------------
%% @doc Compare members in two lists within checking length
-spec compare_members(List1,List2) -> boolean()
	when
	List1 :: list(),
	List2 :: list().

compare_members(List1,List2) ->
	Length1 = length(List1),
	Length2 = length(List2),
	if
		Length1 == Length2 -> compare_members_handler(List1,List2);
		true -> false
	end.


%% ----------------------------
%% @doc Compare members in two lists
-spec compare_members_handler(List1,List2) -> boolean()
	when
	List1 :: list(),
	List2 :: list().

compare_members_handler([],_) -> true;
compare_members_handler([Member|List1],List2) ->
	case lists:member(Member,List2) of
		true -> compare_members_handler(List1,List2);
		_ -> false
	end.


%% ----------------------------
%% @doc Find members of list from another list
-spec find_members(Members,List) -> list()
	when
	Members :: list(),
	List :: list().

find_members(Members_list,List) ->
	find_members_handler(Members_list,List,[]).


%% ----------------------------
%% @doc Find members of list from another list
-spec find_members_handler(Members,List,Output) -> list()
	when
	Members :: list(),
	List :: list(),
	Output :: list().

find_members_handler([],_,Output) -> Output;
find_members_handler([Member|Members],List,Output) ->
	find_members_handler(Members,List,
		case lists:member(Member,List) of
			true -> lists:append(Output,[Member]);
			false -> Output
		end
	).


%% ----------------------------
%% @doc Clear duplicates from defined list
-spec clear_duplicates(List::list()) -> list().

clear_duplicates(List) -> clear_duplicates_handler(List,[]).


%% ----------------------------
%% @doc Clear duplicates from defined list
-spec clear_duplicates_handler(List,Output) -> list()
	when
	List :: list(),
	Output :: list().

clear_duplicates_handler([],Output) -> Output;
clear_duplicates_handler([Element|List],Output) ->
	clear_duplicates_handler(
		List,
		case lists:member(Element,Output) of
			true -> Output;
			false -> lists:append(Output,[Element])
		end
	).

%% ----------------------------
%% @doc Wrapper function for check/3, checking list of typed elements
-spec check(List,Type_properties) -> list() | nomatch
	when
	List :: list(),
	Type_properties :: {Type,Type_parameters},
	Type :: atom(),
	Type_parameters :: list().

check(List,Type_properties) -> check(List,Type_properties,[]).


%% ----------------------------
%% @doc Checking list of typed elements
-spec check(List,Type_properties,Output) -> list() | nomatch
	when
	List :: list(),
	Type_properties :: {Type,Type_parameters},
	Type :: atom(),
	Type_parameters :: list(),
	Output :: list().

check([],_,Output) -> Output;
check([Element|List],{Type,Type_parameters},Output) ->
	case a_params:check(Type,Element,Type_parameters) of
		nomatch -> nomatch;
		Checked_element ->
			check(
				List,{Type,Type_parameters},
				lists:append(Output,[Checked_element])
			)
	end.


%% ----------------------------
%% @doc Get out key-value pair and return cleared List and value
-spec get_out(Type,Key,List) -> Result | {error,_Reason}
	when
	Type :: value | pair,
	Key :: atom(),
	List :: list(),
	Result :: list().

get_out(value,Key,List) ->
	case proplists:get_value(Key,List) of
		undefined -> undefined;
		Value ->
			List_out = proplists:delete(Key,List),
			[Value,List_out]
	end;
get_out(pair,Key,List) ->
	case proplists:get_value(Key,List) of
		undefined -> undefined;
		Value ->
			List_out = proplists:delete(Key,List),
			[{Key,Value},List_out]
	end.