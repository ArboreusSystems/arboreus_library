%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc The string values verified conversion module
%%%
%%% @end
%%% Created : 11/10/2018 at 14:24
%%%-------------------------------------------------------------------
-module(a_value_str_is).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types
-include("../data_models/types/types_general.hrl").
-include("../data_models/types/types_network.hrl").
-include("../data_models/types/types_time.hrl").

%% Data models

%% API
-export([
	test/0,
	by_size/2,by_pattern/2,
	float/1,float_pos/1,float_neg/1,float_from_list/2,float_ranged/3,
	integer/1,integer_pos/1,integer_neg/1,integer_from_list/2,integer_ranged/3,
	atom/1,atom_from_list/2,
	boolean/1,boolean_digit/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_value_str_is) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Float1 = 0.111, Float1_neg = -0.111, Float2 = 0.222, Float3 = 0.333,
	Float1_string = "0.111", Float1_neg_string = "-0.111",
	Float2_string = "0.222", Float3_string = "0.333",
	Float_string_wrong = "Float_binary_wrong",
	{true,Float1} = a_value_str_is:float(Float1_string),
	false = a_value_str_is:float(Float_string_wrong),
	{true,Float1} = a_value_str_is:float_pos(Float1_string),
	false = a_value_str_is:float_pos(Float1_neg_string),
	false = a_value_str_is:float_pos(Float_string_wrong),
	{true,Float1_neg} = a_value_str_is:float_neg(Float1_neg_string),
	false = a_value_str_is:float_neg(Float_string_wrong),
	false = a_value_str_is:float_neg(Float1_string),
	Float_list = [Float1,Float2],
	{true,Float1} = a_value_str_is:float_from_list(Float1_string,Float_list),
	false = a_value_str_is:float_from_list(Float3_string,Float_list),
	false = a_value_str_is:float_from_list(Float_string_wrong,Float_list),
	{true,Float2} = a_value_str_is:float_ranged(Float2_string,Float3,Float1_neg),
	{true,Float2} = a_value_str_is:float_ranged(Float2_string,Float1,Float3),
	{true,Float2} = a_value_str_is:float_ranged(Float2_string,Float1,Float2),
	{true,Float2} = a_value_str_is:float_ranged(Float2_string,Float2,Float3),
	false = a_value_str_is:float_ranged(Float3_string,Float1,Float2),
	false = a_value_str_is:float_ranged(Float_string_wrong,Float1,Float2),
	io:format("DONE! Float values verification test passed.~n"),
	Integer1_string = "111", Integer2_string = "222",
	Integer3_string = "333", Integer_neg_string = "-222",
	Integer_wrong = "Integer_wrong",
	Integer1 = 111, Integer2 = 222, Integer3 = 333,	Integer_neg = -222,
	Integer_list = [Integer1,Integer3],
	{true,Integer1} = a_value_str_is:integer(Integer1_string),
	{true,Integer_neg} = a_value_str_is:integer(Integer_neg_string),
	false = a_value_str_is:integer(Integer_wrong),
	{true,Integer1} = a_value_str_is:integer_pos(Integer1_string),
	false = a_value_str_is:integer_pos(Integer_neg_string),
	false = a_value_str_is:integer_pos(Integer_wrong),
	{true,Integer_neg} = a_value_str_is:integer_neg(Integer_neg_string),
	false = a_value_str_is:integer_neg(Integer1_string),
	false = a_value_str_is:integer_neg(Integer_wrong),
	{true,Integer3} = a_value_str_is:integer_from_list(Integer3_string,Integer_list),
	false = a_value_str_is:integer_from_list(Integer2_string,Integer_list),
	false = a_value_str_is:integer_from_list(Integer_wrong,Integer_list),
	{true,Integer1} = a_value_str_is:integer_ranged(Integer1_string,Integer3,Integer_neg),
	false = a_value_str_is:integer_ranged(Integer_wrong,Integer3,Integer_neg),
	false = a_value_str_is:integer_ranged(Integer1_string,Integer3,Integer2),
	io:format("DONE! Integer values verification test passed.~n"),
	Atom1 = atom1, Atom1_binary = "atom1",
	Atom2 = atom2, Atom2_binary = "atom2",
	Atom3 = atom3, Atom3_binary = "atom3",
	Atom_wrong_binary = "Atom_wrong",
	Atoms = [Atom2,Atom3],
	{true,Atom1} = a_value_str_is:atom(Atom1_binary),
	{true,Atom2} = a_value_str_is:atom(Atom2_binary),
	false = a_value_str_is:atom(Atom_wrong_binary),
	{true,Atom3} = a_value_str_is:atom_from_list(Atom3_binary,Atoms),
	false = a_value_str_is:atom_from_list(Atom_wrong_binary,Atoms),
	false = a_value_str_is:atom_from_list(Atom1_binary,Atoms),
	io:format("DONE! Atom values verification test passed.~n"),
	Boolean1 = true, Boolean1_binary = "true",
	Boolean2 = false, Boolean2_binary = "false",
	Boolean_digit1 = 1, Boolean_digit1_binary = "1",
	Boolean_digit2 = 0, Boolean_digit2_binary = "0",
	Boolean_wrong = "boolean_wrong",
	{true,Boolean1} = a_value_str_is:boolean(Boolean1_binary),
	{true,Boolean2} = a_value_str_is:boolean(Boolean2_binary),
	false = a_value_str_is:boolean(Boolean_wrong),
	{true,Boolean_digit1} = a_value_str_is:boolean_digit(Boolean_digit1_binary),
	{true,Boolean_digit2} = a_value_str_is:boolean_digit(Boolean_digit2_binary),
	false = a_value_str_is:boolean_digit(Boolean_wrong),
	io:format("DONE! Boolean values verification test passed.~n"),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_value_str_is) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Verify digital boolean string value
-spec boolean_digit(Utf_string) -> {true,boolean_digit()} | false
	when
	Utf_string :: utf_text_string().

boolean_digit(Utf_string) -> integer_from_list(Utf_string,[1,0]).


%% ----------------------------
%% @doc Verify boolean string value
-spec boolean(Utf_string) -> {true,boolean()} | false
	when
	Utf_string :: utf_text_string().

boolean(Utf_string) -> atom_from_list(Utf_string,[true,false]).


%% ----------------------------
%% @doc Verify atom string value from list
-spec atom_from_list(Utf_string,List) -> {true,atom()} | false
	when
	Utf_string :: utf_text_string(),
	List :: list_of_atoms().

atom_from_list(Utf_string,List) ->
	case atom(Utf_string) of
		{true,Atom} ->
			case lists:member(Atom,List) of
				true -> {true,Atom};
				_ -> false
			end;
		_ -> false
	end.


%% ----------------------------
%% @doc Verify atom string value
-spec atom(Utf_string) -> {true,atom()} | false
	when
	Utf_string :: utf_text_string().

atom(Utf_string) ->
	Pattern = "^[a-z]{1}[a-zA-Z0-9\_]*$",
	case re:run(Utf_string,Pattern) of
		{match,_} -> {true,list_to_atom(Utf_string)};
		_ -> false
	end.


%% ----------------------------
%% @doc Verify ranged integer string value
-spec integer_ranged(Utf_string,Minor,Major) -> {true,integer()} | false
	when
	Utf_string :: utf_text_string(),
	Minor :: number(),
	Major :: number().

integer_ranged(Utf_string,Range1,Range2) when Range1 > Range2 ->
	integer_ranged(Utf_string,Range2,Range1);
integer_ranged(Utf_string,Minor,Major) ->
	case a_value_str_is:integer(Utf_string) of
		{true,Integer} ->
			if
				Integer =< Major ->
					if
						Integer >= Minor -> {true,Integer};
						true -> false
					end;
				true -> false
			end;
		_ -> false
	end.


%% ----------------------------
%% @doc Verify integer value string from the listed values
-spec integer_from_list(Utf_string,List) -> {true,integer()} | false
	when
	Utf_string :: utf_text_binary(),
	List :: list_of_integers().

integer_from_list(Utf_string,List) ->
	case a_value_str_is:integer(Utf_string) of
		{true,Integer} ->
			case lists:member(Integer,List) of
				true -> {true,Integer};
				_ -> false
			end;
		_ -> false
	end.


%% ----------------------------
%% @doc Verify negative integer string value
-spec integer_neg(Utf_string) -> {true,neg_integer()} | false
	when
	Utf_string :: utf_text_string().

integer_neg(Utf_string) ->
	case a_value_str_is:integer(Utf_string) of
		{true,Integer} ->
			if
				Integer < 0 -> {true,Integer};
				true -> false
			end;
		_ -> false
	end.


%% ----------------------------
%% @doc Verify positive integer string value
-spec integer_pos(Utf_string) -> {true,pos_integer()} | false
	when
	Utf_string :: utf_text_string().

integer_pos(Utf_string) ->
	case a_value_str_is:integer(Utf_string) of
		{true,Integer} ->
			if
				Integer >= 0 -> {true,Integer};
				true -> false
			end;
		_ -> false
	end.


%% ----------------------------
%% @doc Verify integer string value
-spec integer(Utf_string) -> {true,integer()} | false
	when
	Utf_string :: utf_text_string().

integer(Utf_string) ->
	try {true,list_to_integer(Utf_string)}
	catch _:_ -> false end.


%% ----------------------------
%% @doc Verify ranged float value
-spec float_ranged(Utf_string,Range1,Range2) -> {true,float()} | false
	when
	Utf_string :: utf_text_string(),
	Range1 :: number(),
	Range2 :: number().

float_ranged(Binary,Range1,Range2) when Range1 > Range2 ->
	float_ranged(Binary,Range2,Range1);
float_ranged(Binary,Minor,Major) ->
	case a_value_str_is:float(Binary) of
		{true,Float} ->
			if
				Float =< Major ->
					if
						Float >= Minor -> {true,Float};
						true -> false
					end;
				true -> false
			end;
		_ -> false
	end.


%% ----------------------------
%% @doc Verify float string from the listed values
-spec float_from_list(Utf_string,List) -> {true,float()} | false
	when
	Utf_string :: utf_text_string(),
	List :: list_of_floats().

float_from_list(Utf_string,List) ->
	case a_value_str_is:float(Utf_string) of
		{true,Float} ->
			case lists:member(Float,List) of
				true -> {true,Float};
				_ -> false
			end;
		_ -> false
	end.


%% ----------------------------
%% @doc Verify negative float string value
-spec float_neg(Utf_string) -> {true,float_pos()} | false
	when
	Utf_string :: utf_text_string().

float_neg(Utf_string) ->
	case a_value_str_is:float(Utf_string) of
		{true,Float} ->
			if
				Float < 0 -> {true,Float};
				true -> false
			end;
		_ -> false
	end.


%% ----------------------------
%% @doc Verify positive float string value
-spec float_pos(Utf_string) -> {true,float_pos()} | false
	when
	Utf_string :: utf_text_string().

float_pos(Utf_string) ->
	case a_value_str_is:float(Utf_string) of
		{true,Float} ->
			if
				Float >= 0 -> {true,Float};
				true -> false
			end;
		_ -> false
	
	end.


%% ----------------------------
%% @doc Verify float string value
-spec float(Utf_string) -> {true,float()} | false
	when
	Utf_string :: utf_text_string().

float(Utf_string) ->
	try {true,list_to_float(Utf_string)}
	catch _:_ -> false end.


%% ----------------------------
%% @doc Verify string value by regex pattern
-spec by_pattern(Utf_string,Pattern) -> {true,utf_text_string()} | false
	when
	Utf_string :: utf_text_string(),
	Pattern :: utf_text_string().

by_pattern(Utf_string,Pattern) ->
	case re:run(Utf_string,Pattern) of
		{match,_} -> {true,Utf_string};
		_ -> false
	end.


%% ----------------------------
%% @doc Verify string value by size
-spec by_size(Utf_string,Parameters) -> {true,utf_text_string()} | false
	when
	Utf_string :: utf_text_string(),
	Parameters :: {equal,Length} | {less_or_equal,Length} | {more_or_equal,Length} | {ranged,Minimal,Maximal},
	Length :: pos_integer(),
	Minimal :: pos_integer(),
	Maximal :: pos_integer().

by_size(Utf_string,{equal,Length}) ->
	Size = length(Utf_string),
	if
		Size == Length -> {true,Utf_string};
		true -> false
	end;
by_size(Utf_string,{less_or_equal,Length}) ->
	Size = length(Utf_string),
	if
		Size =< Length -> {true,Utf_string};
		true -> false
	end;
by_size(Utf_string,{more_or_equal,Length}) ->
	Size = length(Utf_string),
	if
		Size >= Length -> {true,Utf_string};
		true -> false
	end;
by_size(Utf_string,{ranged,Minimal,Maximal}) ->
	Size = length(Utf_string),
	if
		Size =< Maximal ->
			if
				Size >= Minimal -> {true,Utf_string};
				true -> false
			end;
		true -> false
	end.
