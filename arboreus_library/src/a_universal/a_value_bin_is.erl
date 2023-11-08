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
-include_lib("../include/types/types_general.hrl").
-include_lib("../include/types/types_network.hrl").
-include_lib("../include/types/types_time.hrl").

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
	fqdn/1,
	term/1,
	ipv4/2,ipv6/2,
	numeric/1,numeric_limited/2,
	alphanumeric/1,alphanumeric_limited/2,
	id_numeric/2,id_alphanumeric/2,
	base64/2,base64_limited/3,
	password/3,
	utf_free/1,utf_limited/2,utf_by_pattern/2,
	time_timestamp/1,time_tuple/1,time_ansi/2,time_rfc822/2,time_rfc850/2
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
	Term = [erlang,term],
	Term_binary = <<("[erlang,term].")/utf8>>,
	Term_wrong = <<("wrong_term")/utf8>>,
	{true,Term} = term(Term_binary),
	false = term(Term_wrong),
	io:format("DONE! Erlang Terms values verification test passed.~n"),
	IPv4_tuple = {1,1,1,1},
	IPv4_binary = <<("1.1.1.1")/utf8>>,
	IPv4_integer = 16843009,
	IPv4_wrong = <<("ip_wrong")/utf8>>,
	{true,IPv4_tuple} = ipv4(IPv4_binary,tuple),
	{true,IPv4_integer} = ipv4(IPv4_binary,integer),
	false = ipv4(IPv4_wrong,integer),
	false = ipv4(IPv4_wrong,tuple),
	io:format("DONE! IPv4 values verification test passed.~n"),
	IPv6_tuple = {8193,3512,4515,2519,7988,35374,1952,30301},
	IPv6_binary = <<("2001:0db8:11a3:09d7:1f34:8a2e:07a0:765d")/utf8>>,
	IPv6_integer = 42540766416740939402060931394078537309,
	IPv6_wrong = <<("ip_wrong")/utf8>>,
	{true,IPv6_tuple} = ipv6(IPv6_binary,tuple),
	{true,IPv6_integer} = ipv6(IPv6_binary,integer),
	false = ipv6(IPv6_wrong,integer),
	false = ipv6(IPv6_wrong,tuple),
	io:format("DONE! IPv6 values verification test passed.~n"),
	Numeric1 = <<("12345")/utf8>>,
	Numeric_wrong = <<("numeric_wrong")/utf8>>,
	{true,Numeric1} = numeric(Numeric1),
	false = numeric(Numeric_wrong),
	{true,Numeric1} = numeric_limited(Numeric1,{equal,5}),
	false = numeric_limited(Numeric1,{equal,1}),
	{true,Numeric1} = numeric_limited(Numeric1,{less_or_equal,5}),
	false = numeric_limited(Numeric1,{less_or_equal,4}),
	{true,Numeric1} = numeric_limited(Numeric1,{more_or_equal,5}),
	false = numeric_limited(Numeric1,{more_or_equal,6}),
	{true,Numeric1} = numeric_limited(Numeric1,{ranged,4,6}),
	false = numeric_limited(Numeric1,{ranged,6,7}),
	io:format("DONE! Numeric values verification test passed.~n"),
	Alphanumeric = <<("bNBZOekdc4r71r7C")/utf8>>,
	Alphanumeric_wrong1 = <<("12345")/utf8>>,
	Alphanumeric_wrong2 = <<("bNB Oekdc4r71r7C")/utf8>>,
	{true,Alphanumeric} = alphanumeric(Alphanumeric),
	false = alphanumeric(Alphanumeric_wrong2),
	{true,Alphanumeric} = alphanumeric_limited(Alphanumeric,{equal,16}),
	false = alphanumeric_limited(Alphanumeric,{equal,17}),
	{true,Alphanumeric} = alphanumeric_limited(Alphanumeric,{less_or_equal,16}),
	false = alphanumeric_limited(Alphanumeric,{less_or_equal,15}),
	{true,Alphanumeric} = alphanumeric_limited(Alphanumeric,{more_or_equal,16}),
	false = alphanumeric_limited(Alphanumeric,{more_or_equal,17}),
	{true,Alphanumeric} = alphanumeric_limited(Alphanumeric,{ranged,15,17}),
	false = alphanumeric_limited(Alphanumeric,{ranged,17,18}),
	io:format("DONE! Alphanumeric values verification test passed.~n"),
	{true,Alphanumeric} = id_alphanumeric(Alphanumeric,16),
	false = id_alphanumeric(Alphanumeric_wrong1,16),
	false = id_alphanumeric(Alphanumeric_wrong2,16),
	{true,Numeric1} = id_numeric(Numeric1,5),
	false = id_numeric(Numeric1,6),
	false = id_numeric(Numeric_wrong,5),
	io:format("DONE! ID values verification test passed.~n"),
	Base64_decoded = <<("Base64_binary")/utf8>>,
	Base64_encoded = base64:encode(Base64_decoded),
	Base64_wrong = <<("Base64_wrong")/utf8>>,
	Base64_encoded_size = byte_size(Base64_encoded),
	Base64_decoded_size = byte_size(Base64_decoded),
	{true,Base64_decoded} = base64(Base64_encoded,binary),
	{true,Base64_encoded} = base64(Base64_encoded,base64),
	false = base64(Base64_wrong,binary),
	{true,Base64_decoded} = base64_limited(Base64_encoded,binary,{equal,Base64_decoded_size}),
	false = base64_limited(Base64_encoded,binary,{equal,Base64_decoded_size+1}),
	{true,Base64_encoded} = base64_limited(Base64_encoded,base64,{equal,Base64_encoded_size}),
	false = base64_limited(Base64_encoded,binary,{equal,Base64_encoded_size+1}),
	{true,Base64_decoded} = base64_limited(Base64_encoded,binary,{less_or_equal,Base64_decoded_size}),
	false = base64_limited(Base64_encoded,binary,{less_or_equal,Base64_decoded_size-1}),
	{true,Base64_encoded} = base64_limited(Base64_encoded,base64,{less_or_equal,Base64_encoded_size}),
	false = base64_limited(Base64_encoded,binary,{less_or_equal,Base64_encoded_size-10}),
	{true,Base64_decoded} = base64_limited(Base64_encoded,binary,{more_or_equal,Base64_decoded_size}),
	false = base64_limited(Base64_encoded,binary,{more_or_equal,Base64_decoded_size+1}),
	{true,Base64_encoded} = base64_limited(Base64_encoded,base64,{more_or_equal,Base64_encoded_size}),
	false = base64_limited(Base64_encoded,binary,{more_or_equal,Base64_encoded_size+1}),
	{true,Base64_decoded} = base64_limited(Base64_encoded,binary,{ranged,Base64_decoded_size-1,Base64_decoded_size+1}),
	false = base64_limited(Base64_encoded,binary,{ranged,Base64_decoded_size+1,Base64_decoded_size+2}),
	{true,Base64_encoded} = base64_limited(Base64_encoded,base64,{ranged,Base64_encoded_size-1,Base64_encoded_size+1}),
	false = base64_limited(Base64_encoded,binary,{ranged,Base64_encoded_size+1,Base64_encoded_size+2}),
	io:format("DONE! Base64 values verification test passed.~n"),
	Password_decoded = <<("qwerty")/utf8>>,
	Password_encoded = base64:encode(Password_decoded),
	Password_wrong = <<("password_wrong")/utf8>>,
	{true,Password_decoded} = password(Password_encoded,4,8),
	false = password(Password_encoded,1,2),
	false = password(Password_wrong,4,8),
	io:format("DONE! Password values verification test passed.~n"),
	UTF_binary = <<("utf_binary")/utf8>>,
	UTF_size = byte_size(UTF_binary),
	UTF_binary_wrong = utf_wrong,
	{true,UTF_binary} = utf_free(UTF_binary),
	false = utf_free(UTF_binary_wrong),
	{true,UTF_binary} = utf_limited(UTF_binary,{equal,UTF_size}),
	false = utf_limited(UTF_binary,{equal,UTF_size-1}),
	{true,UTF_binary} = utf_limited(UTF_binary,{less_or_equal,UTF_size}),
	false = utf_limited(UTF_binary,{less_or_equal,UTF_size-1}),
	{true,UTF_binary} = utf_limited(UTF_binary,{more_or_equal,UTF_size}),
	false = utf_limited(UTF_binary,{more_or_equal,UTF_size+1}),
	{true,UTF_binary} = utf_limited(UTF_binary,{ranged,UTF_size-1,UTF_size+1}),
	false = utf_limited(UTF_binary,{ranged,UTF_size+1,UTF_size+2}),
	{true,UTF_binary} = utf_by_pattern(UTF_binary,<<("^.{1,}$")/utf8>>),
	false = utf_by_pattern(UTF_binary,<<("^[A]{1}$")/utf8>>),
	io:format("DONE! UTF binary verification test passed.~n"),
	Time_tuple = erlang:localtime(),
	Time_tuple_binary = a_term:to_utf_binary(Time_tuple),
	Time_tuple_binary_wrong = <<("wrong_time_tuple")/utf8>>,
	{true,Time_tuple} = time_tuple(Time_tuple_binary),
	false = time_tuple(Time_tuple_binary_wrong),
	Time_timestamp = a_time:timestamp(),
	Time_timestamp_binary = integer_to_binary(Time_timestamp),
	Time_timestamp_wrong = <<("wrong_timestamp")/utf8>>,
	{true,Time_timestamp} = time_timestamp(Time_timestamp_binary),
	false = time_timestamp(Time_timestamp_wrong),
	Time_ansi = a_time:format(ansi,{date_tuple,Time_tuple}),
	Time_rfc850 = a_time:format(rfc850,{date_tuple,Time_tuple}),
	Time_rfc822 = a_time:format(rfc822,{date_tuple,Time_tuple}),
	{true,Time_tuple} = time_ansi(Time_ansi,tuple),
	false = time_ansi(Time_tuple_binary_wrong,tuple),
	{true,Time_tuple} = time_rfc850(Time_rfc850,tuple),
	false = time_rfc850(Time_tuple_binary_wrong,tuple),
	{true,Time_tuple} = time_rfc822(Time_rfc822,tuple),
	false = time_rfc822(Time_tuple_binary_wrong,tuple),
	io:format("DONE! Time verification test passed.~n"),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_value_bin_is) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Verify time-tuple binary value
-spec time_tuple(Binary) -> {true,a_time_tuple()} | false
	when
	Binary :: utf_text_binary().

time_tuple(Binary) ->
	try
		{{Year,Month,Day},{Hours,Minutes,Seconds}} = a_term:from_utf_binary(Binary),
		{true,{{Year,Month,Day},{Hours,Minutes,Seconds}}}
	catch
		_:_  -> false
	end.


%% ----------------------------
%% @doc Verify UNIX-timestamp value
-spec time_timestamp(Binary) -> {true,pos_integer()} | false
	when
	Binary :: utf_text_binary().

time_timestamp(Binary) -> integer_pos(Binary).


%% ----------------------------
%% @doc Verify rfc822 unicode time value
-spec time_rfc822(Binary,Return_mode) -> {true,Time} | false
	when
	Binary :: utf_text_binary(),
	Return_mode :: tuple | seconds | timestamp,
	Time :: pos_integer() | a_time_tuple().

time_rfc822(Binary,Return_mode) ->
	case a_time:from_formated(rfc822,Binary,Return_mode) of
		{error,_} -> false;
		false -> false;
		Result -> {true,Result}
	end.


%% ----------------------------
%% @doc Verify rfc850 unicode time value
-spec time_rfc850(Binary,Return_mode) -> {true,Time} | false
	when
	Binary :: utf_text_binary(),
	Return_mode :: tuple | seconds | timestamp,
	Time :: pos_integer() | a_time_tuple().

time_rfc850(Binary,Return_mode) ->
	case a_time:from_formated(rfc850,Binary,Return_mode) of
		{error,_} -> false;
		false -> false;
		Result -> {true,Result}
	end.


%% ----------------------------
%% @doc Verify ANSI unicode time value
-spec time_ansi(Binary,Return_mode) -> {true,Time} | false
	when
	Binary :: utf_text_binary(),
	Return_mode :: tuple | seconds | timestamp,
	Time :: pos_integer() | a_time_tuple().

time_ansi(Binary,Return_mode) ->
	case a_time:from_formated(ansi,Binary,Return_mode) of
		{error,_} -> false;
		false -> false;
		Result -> {true,Result}
	end.


%% ----------------------------
%% @doc Verify unicode binary by pattern
-spec utf_by_pattern(Binary,Pattern) -> {true,utf_text_binary()} | false
	when
	Binary :: utf_text_binary(),
	Pattern :: utf_text_binary().

utf_by_pattern(Binary,Pattern) ->
	case utf_free(Binary) of
		{true,Binary} -> by_pattern(Binary,Pattern);
		Result -> Result
	end.


%% ----------------------------
%% @doc Verify limited unicode binary
-spec utf_limited(Binary,Limit) -> {true,utf_text_binary()} | false
	when
	Binary :: utf_text_binary(),
	Limit :: {equal,Length} | {less_or_equal,Length} | {more_or_equal,Length} | {ranged,Minimal,Maximal},
	Length :: pos_integer(),
	Minimal :: pos_integer(),
	Maximal :: pos_integer().

utf_limited(Binary,Limit) ->
	case utf_free(Binary) of
		{true,Binary} -> by_size(Binary,Limit);
		Result -> Result
	end.


%% ----------------------------
%% @doc Verify unicode binary
-spec utf_free(Binary) -> {true,utf_text_binary()} | false
	when
	Binary :: utf_text_binary().

utf_free(Binary) when is_binary(Binary) -> {true,Binary};
utf_free(_) -> false.


%% ----------------------------
%% @doc Verify password value
-spec password(Binary,Minimal_length,Maximal_length) -> {true,utf_text_binary()} | false
	when
	Binary :: utf_base64_binary(),
	Minimal_length :: pos_integer(),
	Maximal_length :: pos_integer().

password(Binary,Minimal,Maximal) ->
	base64_limited(Binary,binary,{ranged,Minimal,Maximal}).


%% ----------------------------
%% @doc Verify Base64 limited value
-spec base64_limited(Binary,Return_mode,Limit) -> {true,Result} | false
	when
	Binary :: utf_base64_binary(),
	Return_mode :: base64 | binary,
	Limit :: {equal,Length} | {less_or_equal,Length} | {more_or_equal,Length} | {ranged,Minimal,Maximal},
	Length :: pos_integer(),
	Minimal :: pos_integer(),
	Maximal :: pos_integer(),
	Result :: utf_base64_binary() | utf_text_binary().

base64_limited(Binary,Return_mode,Limit) ->
	case Return_mode of
		binary ->
			case base64(Binary,Return_mode) of
				{true,Encoded_binary} -> by_size(Encoded_binary,Limit);
				Result -> Result
			end;
		_ ->
			case by_size(Binary,Limit) of
				{true,Binary} -> base64(Binary,base64);
				Result -> Result
			end
	end.


%% ----------------------------
%% @doc Verify Base64 value
-spec base64(Binary,Return_mode) -> {true,Result} | false
	when
	Binary :: utf_base64_binary(),
	Return_mode :: base64 | binary,
	Result :: utf_base64_binary() | utf_text_binary().

base64(Binary,Return_mode) ->
	try
		Encoded_binary = base64:decode(Binary),
		case Return_mode of
			base64 -> {true,Binary};
			_ -> {true,Encoded_binary}
		end
	catch _:_ -> false end.


%% ----------------------------
%% @doc Verify alphanumeric ID value
-spec id_alphanumeric(Binary,Length) -> {true,utf_text_binary()} | false
	when
	Binary :: utf_text_binary(),
	Length :: pos_integer().

id_alphanumeric(Binary,Length) -> alphanumeric_limited(Binary,{equal,Length}).


%% ----------------------------
%% @doc Verify numeric ID value
-spec id_numeric(Binary,Length) -> {true,utf_text_binary()} | false
	when
	Binary :: utf_text_binary(),
	Length :: pos_integer().

id_numeric(Binary,Length) -> numeric_limited(Binary,{equal,Length}).


%% ----------------------------
%% @doc Verify limited alphanumeric value
-spec alphanumeric_limited(Binary,Limit) -> {true,utf_text_binary()} | false
	when
	Binary :: utf_text_binary(),
	Limit :: {equal,Length} | {less_or_equal,Length} | {more_or_equal,Length} | {ranged,Minimal,Maximal},
	Length :: pos_integer(),
	Minimal :: pos_integer(),
	Maximal :: pos_integer().

alphanumeric_limited(Binary,Limit) ->
	case by_size(Binary,Limit) of
		{true,Binary} -> alphanumeric(Binary);
		Result -> Result
	end.


%% ----------------------------
%% @doc Verify alphanumeric value
-spec alphanumeric(Binary) -> {true,utf_text_binary()} | false
	when
	Binary :: utf_text_binary().

alphanumeric(Binary) ->
	by_pattern(Binary,<<("^[a-zA-Z0-9]{1,}$")/utf8>>).


%% ----------------------------
%% @doc Verify limited numeric value
-spec numeric_limited(Binary,Limit) -> {true,utf_text_binary()} | false
	when
	Binary :: utf_text_binary(),
	Limit :: {equal,Length} | {less_or_equal,Length} | {more_or_equal,Length} | {ranged,Minimal,Maximal},
	Length :: pos_integer(),
	Minimal :: pos_integer(),
	Maximal :: pos_integer().

numeric_limited(Binary,Limit) ->
	case by_size(Binary,Limit) of
		{true,Binary} -> numeric(Binary);
		_ -> false
	end.


%% ----------------------------
%% @doc Verify numeric value
-spec numeric(Binary) -> {true,utf_text_binary()} | false
	when
	Binary :: utf_text_binary().

numeric(Binary) ->
	by_pattern(Binary,<<("^[0-9]{1,}$")/utf8>>).


%% ----------------------------
%% @doc Verify IPv6 value
-spec ipv6(Binary,Return_mode) -> {true,Ip} | false
	when
	Binary :: utf_text_binary(),
	Return_mode :: integer | tuple,
	Ip :: ipv6_tuple() | ipv4_integer().

ipv6(Binary,Return_mode) ->
	Ip_string = unicode:characters_to_list(Binary),
	try
		case Return_mode of
			integer ->
				{true,a_net:ipv6_to_integer(Ip_string)};
			_ ->
				{ok,Ip_tuple} = inet:parse_ipv6_address(Ip_string),
				{true,Ip_tuple}
		end
	catch
		_:_ -> false
	end.
	

%% ----------------------------
%% @doc Verify IPv4 value
-spec ipv4(Binary,Return_mode) -> {true,Ip} | false
	when
	Binary :: utf_text_binary(),
	Return_mode :: integer | tuple,
	Ip :: ipv4_integer() | ipv4_tuple().

ipv4(Binary,Return_mode) ->
	try
		{ok,Ip_tuple} = inet:parse_ipv4_address(unicode:characters_to_list(Binary)),
		case Return_mode of
			integer -> {true,a_net:ipv4_to_integer(Ip_tuple)};
			_ -> {true,Ip_tuple}
		end
	catch
		_:_ -> false
	end.


%% ----------------------------
%% @doc Verify Erlang term value
-spec term(Byte) -> {true,term()} | false
	when
	Byte :: byte().

term(Byte) ->
	try {true,a_term:from_utf_binary(Byte)}
	catch _:_ -> false end.


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
%% @doc Verify latin name value limited by length
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
%% @doc Verify positive integer binary value
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