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
	float/1,float_pos/1,float_neg/1,float_from_list/2,float_ranged/3
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
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_value_bin_is) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


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