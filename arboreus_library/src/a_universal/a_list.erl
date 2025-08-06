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
-include("a_includes.hrl").

%% Data models

%% API
-export([

	test/0,

	get_out/3,
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
-spec numerate(LIST) -> list()
	when LIST :: list().

numerate(LIST) -> numerate_handler(LIST,1,[]).


%% ----------------------------
%% @doc Numerate procedure handler
-spec numerate_handler(LIST,COUNT,OUTPUT) -> OUTPUT
	when
		LIST :: list(),
		COUNT :: pos_integer(),
		OUTPUT :: list().

numerate_handler([],_,OUTPUT) -> OUTPUT;

numerate_handler([VALUE|LIST],COUNT,OUTPUT) ->

	numerate_handler(
		LIST,COUNT + 1,
		lists:append(OUTPUT,[{COUNT,VALUE}])
	).


%% ----------------------------
%% @doc Exclude members of list from another lists and return diff
-spec exclude(LIST,MEMBERS) -> list()
	when
		LIST :: list(),
		MEMBERS :: list().

exclude(LIST,MEMBERS) -> exclude_handler(LIST,MEMBERS,[]).


%% ----------------------------
%% @doc Aux function for exclude/2
-spec exclude_handler(LIST,MEMBERS,OUTPUT) -> list()
	when
		LIST :: list(),
		MEMBERS :: list(),
		OUTPUT :: list().

exclude_handler([],_,OUTPUT) -> OUTPUT;

exclude_handler([ELEMENT|LIST],MEMBERS,OUTPUT) ->

	exclude_handler(
		LIST,MEMBERS,
		case lists:member(ELEMENT,MEMBERS) of
			false -> lists:append(OUTPUT,[ELEMENT]);
			_ -> OUTPUT
		end
	).


%% ----------------------------
%% @doc Compare members in two lists within checking length
-spec compare_members(LIST1,LIST2) -> boolean()
	when
		LIST1 :: list(),
		LIST2 :: list().

compare_members(LIST1,LIST2) ->

	LENGTH1 = length(LIST1),
	LENGTH2 = length(LIST2),
	if
		LENGTH1 == LENGTH2 -> compare_members_handler(LIST1,LIST2);
		true -> false
	end.


%% ----------------------------
%% @doc Compare members in two lists
-spec compare_members_handler(LIST1,LIST2) -> boolean()
	when
		LIST1 :: list(),
		LIST2 :: list().

compare_members_handler([],_) -> true;

compare_members_handler([MEMBER|LIST1],LIST2) ->

	case lists:member(MEMBER,LIST2) of
		true -> compare_members_handler(LIST1,LIST2);
		_ -> false
	end.


%% ----------------------------
%% @doc Find members of list from another list
-spec find_members(MEMBERS,LIST) -> list()
	when
		MEMBERS :: list(),
		LIST :: list().

find_members(MEMBERS,LIST) ->

	find_members_handler(MEMBERS,LIST,[]).


%% ----------------------------
%% @doc Find members of list from another list
-spec find_members_handler(MEMBERS,LIST,OUTPUT) -> list()
	when
		MEMBERS :: list(),
		LIST :: list(),
		OUTPUT :: list().

find_members_handler([],_,OUTPUT) -> OUTPUT;

find_members_handler([MEMBER|MEMBERS],LIST,OUTPUT) ->

	find_members_handler(MEMBERS,LIST,
		case lists:member(MEMBER,LIST) of
			true -> lists:append(OUTPUT,[MEMBER]);
			false -> OUTPUT
		end
	).


%% ----------------------------
%% @doc Clear duplicates from defined list
-spec clear_duplicates(LIST ::list()) -> list().

clear_duplicates(LIST) -> clear_duplicates_handler(LIST,[]).


%% ----------------------------
%% @doc Clear duplicates from defined list
-spec clear_duplicates_handler(LIST,OUTPUT) -> list()
	when
		LIST :: list(),
		OUTPUT :: list().

clear_duplicates_handler([],OUTPUT) -> OUTPUT;

clear_duplicates_handler([ELEMENT|LIST],OUTPUT) ->

	clear_duplicates_handler(
		LIST,
		case lists:member(ELEMENT,OUTPUT) of
			true -> OUTPUT;
			false -> lists:append(OUTPUT,[ELEMENT])
		end
	).


%% ----------------------------
%% @doc Get out key-value pair and return cleared List and value
-spec get_out(TYPE,KEY,LIST) -> RESULT | {error,REASON}
	when
		TYPE :: value | pair,
		KEY :: atom(),
		LIST :: list(),
		RESULT :: list(),
		REASON :: term().

get_out(value,KEY,LIST) ->

	case proplists:get_value(KEY,LIST) of
		undefined -> undefined;
		VALUE ->
			LIST_OUT = proplists:delete(KEY,LIST),
			[VALUE,LIST_OUT]
	end;

get_out(pair,KEY,LIST) ->

	case proplists:get_value(KEY,LIST) of
		undefined -> undefined;
		VALUE ->
			LIST_OUT = proplists:delete(KEY,LIST),
			[{KEY,VALUE},LIST_OUT]
	end.
