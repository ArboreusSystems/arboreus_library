%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc The binary values verified conversion module
%%%
%%% @end
%%% Created : 10/29/2018 at 16:01
%%%-------------------------------------------------------------------
-module(a_value_bin_is).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types
-include("../data_models/types/types_general.hrl").

%% Data models

%% API
-export([
	test/0,
	integer/1,integer_pos/1,integer_neg/1,integer_from_list/2,integer_ranged/3,
	float/1,float_pos/1,float_neg/1,float_from_list/2,float_ranged/3,
	atom/1,atom_from_list/2,
	boolean/1,boolean_digit/1,
	by_pattern/2,by_size/2,
	latin_name/1,latin_name_limited/2,
	email/1,
	fqdn/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_value_bin_is) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Float1 = 0.111,
	Float1_neg = -0.111,
	Float2 = 0.222,
	Float3 = 0.333,
	Float1_binary = <<("0.111")/utf8>>,
	Float1_neg_binary = <<("-0.111")/utf8>>,
	Float2_binary = <<("0.222")/utf8>>,
	Float3_binary = <<("0.333")/utf8>>,
	Float_binary_wrong = <<("Float_binary_wrong")/utf8>>,
	{true,Float1} = a_value_bin_is:float(Float1_binary),
	false = a_value_bin_is:float(Float_binary_wrong),
	{true,Float1} = a_value_bin_is:float_pos(Float1_binary),
	false = a_value_bin_is:float_pos(Float1_neg_binary),
	false = a_value_bin_is:float_pos(Float_binary_wrong),
	{true,Float1_neg} = a_value_bin_is:float_neg(Float1_neg_binary),
	false = a_value_bin_is:float_neg(Float_binary_wrong),
	false = a_value_bin_is:float_neg(Float1_binary),
	Float_list = [Float1,Float2],
	{true,Float1} = a_value_bin_is:float_from_list(Float1_binary,Float_list),
	false = a_value_bin_is:float_from_list(Float3_binary,Float_list),
	false = a_value_bin_is:float_from_list(Float_binary_wrong,Float_list),
	{true,Float2} = a_value_bin_is:float_ranged(Float2_binary,Float3,Float1_neg),
	{true,Float2} = a_value_bin_is:float_ranged(Float2_binary,Float1,Float3),
	{true,Float2} = a_value_bin_is:float_ranged(Float2_binary,Float1,Float2),
	{true,Float2} = a_value_bin_is:float_ranged(Float2_binary,Float2,Float3),
	false = a_value_bin_is:float_ranged(Float3_binary,Float1,Float2),
	false = a_value_bin_is:float_ranged(Float_binary_wrong,Float1,Float2),
	io:format("DONE! Float values verification test passed.~n"),
	Integer1_binary = <<("111")/utf8>>,
	Integer2_binary = <<("222")/utf8>>,
	Integer3_binary = <<("333")/utf8>>,
	Integer_neg_binary = <<("-222")/utf8>>,
	Integer_wrong = <<("Integer_wrong")/utf8>>,
	Integer1 = 111,
	Integer2 = 222,
	Integer3 = 333,
	Integer_neg = -222,
	Integer_list = [Integer1,Integer3],
	{true,Integer1} = a_value_bin_is:integer(Integer1_binary),
	{true,Integer_neg} = a_value_bin_is:integer(Integer_neg_binary),
	false = a_value_bin_is:integer(Integer_wrong),
	{true,Integer1} = a_value_bin_is:integer_pos(Integer1_binary),
	false = a_value_bin_is:integer_pos(Integer_neg_binary),
	false = a_value_bin_is:integer_pos(Integer_wrong),
	{true,Integer_neg} = a_value_bin_is:integer_neg(Integer_neg_binary),
	false = a_value_bin_is:integer_neg(Integer1_binary),
	false = a_value_bin_is:integer_neg(Integer_wrong),
	{true,Integer3} = a_value_bin_is:integer_from_list(Integer3_binary,Integer_list),
	false = a_value_bin_is:integer_from_list(Integer2_binary,Integer_list),
	false = a_value_bin_is:integer_from_list(Integer_wrong,Integer_list),
	{true,Integer1} = a_value_bin_is:integer_ranged(Integer1_binary,Integer3,Integer_neg),
	false = a_value_bin_is:integer_ranged(Integer_wrong,Integer3,Integer_neg),
	false = a_value_bin_is:integer_ranged(Integer1_binary,Integer3,Integer2),
	io:format("DONE! Integer values verification test passed.~n"),
	Atom1 = atom1, Atom1_binary = <<("atom1")/utf8>>,
	Atom2 = atom2, Atom2_binary = <<("atom2")/utf8>>,
	Atom3 = atom3, Atom3_binary = <<("atom3")/utf8>>,
	Atom_wrong_binary = <<("Atom_wrong")/utf8>>,
	Atoms = [Atom2,Atom3],
	{true,Atom1} = a_value_bin_is:atom(Atom1_binary),
	{true,Atom2} = a_value_bin_is:atom(Atom2_binary),
	false = a_value_bin_is:atom(Atom_wrong_binary),
	{true,Atom3} = a_value_bin_is:atom_from_list(Atom3_binary,Atoms),
	false = a_value_bin_is:atom_from_list(Atom_wrong_binary,Atoms),
	false = a_value_bin_is:atom_from_list(Atom1_binary,Atoms),
	io:format("DONE! Atom values verification test passed.~n"),
	Boolean1 = true, Boolean1_binary = <<("true")/utf8>>,
	Boolean2 = false, Boolean2_binary = <<("false")/utf8>>,
	Boolean_digit1 = 1, Boolean_digit1_binary = <<("1")/utf8>>,
	Boolean_digit2 = 0, Boolean_digit2_binary = <<("0")/utf8>>,
	Boolean_wrong = <<("boolean_wrong")/utf8>>,
	{true,Boolean1} = a_value_bin_is:boolean(Boolean1_binary),
	{true,Boolean2} = a_value_bin_is:boolean(Boolean2_binary),
	false = a_value_bin_is:boolean(Boolean_wrong),
	{true,Boolean_digit1} = a_value_bin_is:boolean_digit(Boolean_digit1_binary),
	{true,Boolean_digit2} = a_value_bin_is:boolean_digit(Boolean_digit2_binary),
	false = a_value_bin_is:boolean_digit(Boolean_wrong),
	io:format("DONE! Boolean values verification test passed.~n"),
	Latin_name = <<("Vasya Pukin")/utf8>>,
	{true,Latin_name} = latin_name(Latin_name),
	false = latin_name(Boolean_wrong),
	{true,Latin_name} = latin_name_limited(Latin_name,{less_or_equal,50}),
	{true,Latin_name} = latin_name_limited(Latin_name,{more_or_equal,5}),
	{true,Latin_name} = latin_name_limited(Latin_name,{equal,11}),
	{true,Latin_name} = latin_name_limited(Latin_name,{ranged,1,100}),
	false = latin_name_limited(Latin_name,{equal,5}),
	false = latin_name_limited(Latin_name,{less_or_equal,5}),
	false = latin_name_limited(Latin_name,{more_or_equal,50}),
	false = latin_name_limited(Latin_name,{ranged,100,1000}),
	io:format("DONE! Latin Name values verification test passed.~n"),
	Email = <<("test@arboreus.systems")/utf8>>,
	{true,Email} = email(Email),
	false = email(Latin_name),
	io:format("DONE! Email values verification test passed.~n"),
	Fqdn1 = <<("fqdn1765_098.test_domain.example.com.")/utf8>>,
	Fqdn_wrong = <<("wrong_fqdn")/utf8>>,
	{true,Fqdn1} = fqdn(Fqdn1),
	false = fqdn(Fqdn_wrong),
	io:format("DONE! FQDN values verification test passed.~n"),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_value_bin_is) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Verify FQND value
-spec fqdn(Binary) -> {true,utf_text_binary()} | false
	when
	Binary :: utf_text_binary().

fqdn(Binary) ->
	case by_size(Binary,{less_or_equal,255}) of
		{true,Binary} -> by_pattern(Binary,<<("^([a-z0-9\-\ \_]{1,}[\.]{1}){1,}$")/utf8>>);
		Result -> Result
	end.


%% ----------------------------
%% @doc Verify email by pattern
-spec email(Binary) -> {true,utf_text_binary()} | false
	when
	Binary :: utf_text_binary().

email(Binary) ->
	by_pattern(Binary,<<("^([a-z0-9\.\_\-]{1,})\@([a-z0-9\.\_\-]{1,})$")/utf8>>).


%% ----------------------------
%% @doc Verify latin name value limited by length by pattern
-spec latin_name_limited(Binary,Limit) -> {true,utf_text_binary()} | false
	when
	Binary :: utf_text_binary(),
	Limit :: {equal,Length} | {less_or_equal,Length} | {more_or_equal,Length} | {ranged,Minimal,Maximal},
	Length :: pos_integer(),
	Minimal :: pos_integer(),
	Maximal :: pos_integer().

latin_name_limited(Binary,Limit) ->
	case by_size(Binary,Limit) of
		{true,Binary} -> latin_name(Binary);
		Result -> Result
	end.


%% ----------------------------
%% @doc Verify latin name binary by pattern
-spec latin_name(Binary) -> {true,utf_text_binary()} | false
	when
	Binary :: utf_text_binary().

latin_name(Binary) ->
	by_pattern(Binary,<<("^(\ ?[A-Z]{1}[a-z]{0,}){1,}$")/utf8>>).


%% ----------------------------
%% @doc verify value by regex pattern
-spec by_pattern(Binary,Pattern) -> {true,utf_text_binary()} | false
	when
	Binary :: utf_text_binary(),
	Pattern :: utf_text_binary().

by_pattern(Binary,Pattern) ->
	case re:run(Binary,Pattern) of
		{match,_} -> {true,Binary};
		_ -> false
	end.


%% ----------------------------
%% @doc Verify binary value by size
-spec by_size(Binary,Parameters) -> {true,utf_text_binary()} | false
	when
	Binary :: utf_text_binary(),
	Parameters :: {equal,Length} | {less_or_equal,Length} | {more_or_equal,Length} | {ranged,Minimal,Maximal},
	Length :: pos_integer(),
	Minimal :: pos_integer(),
	Maximal :: pos_integer().

by_size(Binary,{equal,Length}) ->
	Size = byte_size(Binary),
	if
		Size == Length -> {true,Binary};
		true -> false
	end;
by_size(Binary,{less_or_equal,Length}) ->
	Size = byte_size(Binary),
	if
		Size =< Length -> {true,Binary};
		true -> false
	end;
by_size(Binary,{more_or_equal,Length}) ->
	Size = byte_size(Binary),
	if
		Size >= Length -> {true,Binary};
		true -> false
	end;
by_size(Binary,{ranged,Minimal,Maximal}) ->
	Size = byte_size(Binary),
	if
		Size =< Maximal ->
			if
				Size >= Minimal -> {true,Binary};
				true -> false
			end;
		true -> false
	end.


%% ----------------------------
%% @doc Verify digital boolean value
-spec boolean_digit(Binary) -> {true,boolean_digit()} | false
	when
	Binary :: utf_text_binary().

boolean_digit(Binary) -> integer_from_list(Binary,[1,0]).


%% ----------------------------
%% @doc Verify boolean value
-spec boolean(Binary) -> {true,boolean()} | false
	when
	Binary :: utf_text_binary().

boolean(Binary) -> atom_from_list(Binary,[true,false]).


%% ----------------------------
%% @doc Verify atom value
-spec atom(Binary) -> {true,atom()} | false
	when
	Binary :: utf_text_binary().

atom(Binary) ->
	Pattern = <<("^[a-z]{1}[a-zA-Z0-9\_]*$")/utf8>>,
	case re:run(Binary,Pattern) of
		{match,_} -> {true,binary_to_atom(Binary,utf8)};
		_ -> false
	end.


%% ----------------------------
%% @doc Verify atom from list membership
-spec atom_from_list(Binary,List) -> {true,atom()} | false
	when
	Binary :: utf_text_binary(),
	List :: list_of_atoms().

atom_from_list(Binary,List) ->
	case atom(Binary) of
		{true,Atom} ->
			case lists:member(Atom,List) of
				true -> {true,Atom};
				_ -> false
			end;
		_ -> false
	end.


%% ----------------------------
%% @doc Verify integer value
-spec integer(Binary) -> {true,integer()} | false
	when
	Binary :: utf_text_binary().

integer(Binary) ->
	try {true,binary_to_integer(Binary)}
	catch _:_ -> false end.


%% ----------------------------
%% @doc verify positive integer value
-spec integer_pos(Binary) -> {true,pos_integer()} | false
	when
	Binary :: utf_text_binary().

integer_pos(Binary) ->
	case a_value_bin_is:integer(Binary) of
		{true,Integer} ->
			if
				Integer >= 0 -> {true,Integer};
				true -> false
			end;
		_ -> false
	end.


%% ----------------------------
%% @doc Verify negative integer value
-spec integer_neg(Binary) -> {true,neg_integer()} | false
	when
	Binary :: utf_text_binary().

integer_neg(Binary) ->
	case a_value_bin_is:integer(Binary) of
		{true,Integer} ->
			if
				Integer < 0 -> {true,Integer};
				true -> false
			end;
		_ -> false
	end.


%% ----------------------------
%% @doc Verify integer value for list membership
-spec integer_from_list(Binary,List) -> {true,integer()} | false
	when
	Binary :: utf_text_binary(),
	List :: list_of_integers().

integer_from_list(Binary,List) ->
	case a_value_bin_is:integer(Binary) of
		{true,Integer} ->
			case lists:member(Integer,List) of
				true -> {true,Integer};
				_ -> false
			end;
		_ -> false
	end.


%% ----------------------------
%% @doc Verify ranged integer value
-spec integer_ranged(Binary,Minor,Major) -> {true,integer()} | false
	when
	Binary :: utf_text_binary(),
	Minor :: number(),
	Major :: number().

integer_ranged(Binary,Range1,Range2) when Range1 > Range2 ->
	integer_ranged(Binary,Range2,Range1);
integer_ranged(Binary,Minor,Major) ->
	case a_value_bin_is:integer(Binary) of
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
%% @doc Verify float value
-spec float(Binary) -> {true,float()} | false
	when
	Binary :: utf_text_binary().

float(Binary) ->
	try {true,binary_to_float(Binary)}
	catch _:_ -> false end.


%% ----------------------------
%% @doc Verify positive float value
-spec float_pos(Binary) -> {true,float_pos()} | false
	when
	Binary :: utf_text_binary().

float_pos(Binary) ->
	case a_value_bin_is:float(Binary) of
		{true,Float} ->
			if
				Float >= 0 -> {true,Float};
				true -> false
			end;
		_ -> false
	end.


%% ----------------------------
%% @doc Verify positive float value
-spec float_neg(Binary) -> {true,float_neg()} | false
	when
	Binary :: utf_text_binary().

float_neg(Binary) ->
	case a_value_bin_is:float(Binary) of
		{true,Float} ->
			if
				Float < 0 -> {true,Float};
				true -> false
			end;
		_ -> false
	end.


%% ----------------------------
%% @doc Verify float value for list membership.
-spec float_from_list(Binary,List) -> {true,float()} | false
	when
	Binary :: utf_text_binary(),
	List :: list_of_floats().

float_from_list(Binary,List) ->
	case a_value_bin_is:float(Binary) of
		{true,Float} ->
			case lists:member(Float,List) of
				true -> {true,Float};
				_ -> false
			end;
		_ -> false
	end.


%% ----------------------------
%% @doc Verify ranged float value
-spec float_ranged(Binary,Range1,Range2) -> {true,float()} | false
	when
	Binary :: utf_text_binary(),
	Range1 :: number(),
	Range2 :: number().

float_ranged(Binary,Range1,Range2) when Range1 > Range2 ->
	float_ranged(Binary,Range2,Range1);
float_ranged(Binary,Minor,Major) ->
	case a_value_bin_is:float(Binary) of
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