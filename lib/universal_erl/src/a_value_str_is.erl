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
-include("../../data_models/types/types_general.hrl").
-include("../../data_models/types/types_network.hrl").
-include("../../data_models/types/types_time.hrl").

%% Data models

%% API
-export([
	test/0,
	by_size/2,by_pattern/2,
	float/1,float_pos/1,float_neg/1,float_from_list/2,float_ranged/3,
	integer/1,integer_pos/1,integer_neg/1,integer_from_list/2,integer_ranged/3,
	atom/1,atom_from_list/2,
	boolean/1,boolean_digit/1,
	latin_name/1,latin_name_limited/2,
	email/1,
	fqdn/1,
	ipv4/2,ipv6/2,
	numeric/1,numeric_limited/2,
	alphanumeric/1,alphanumeric_limited/2,
	id_numeric/2,id_alphanumeric/2,
	base64/2,base64_limited/3,
	password/3,
	utf_free/1,utf_limited/2,utf_by_pattern/2,
	time_ansi/2,time_rfc850/2,time_rfc822/2,time_tuple/1,time_timestamp/1
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
	Atom1 = atom1, Atom1_string = "atom1",
	Atom2 = atom2, Atom2_string = "atom2",
	Atom3 = atom3, Atom3_string = "atom3",
	Atom_wrong_binary = "Atom_wrong",
	Atoms = [Atom2,Atom3],
	{true,Atom1} = a_value_str_is:atom(Atom1_string),
	{true,Atom2} = a_value_str_is:atom(Atom2_string),
	false = a_value_str_is:atom(Atom_wrong_binary),
	{true,Atom3} = a_value_str_is:atom_from_list(Atom3_string,Atoms),
	false = a_value_str_is:atom_from_list(Atom_wrong_binary,Atoms),
	false = a_value_str_is:atom_from_list(Atom1_string,Atoms),
	io:format("DONE! Atom values verification test passed.~n"),
	Boolean1 = true, Boolean1_string = "true",
	Boolean2 = false, Boolean2_string = "false",
	Boolean_digit1 = 1, Boolean_digit1_string = "1",
	Boolean_digit2 = 0, Boolean_digit2_string = "0",
	Boolean_wrong = "boolean_wrong",
	{true,Boolean1} = a_value_str_is:boolean(Boolean1_string),
	{true,Boolean2} = a_value_str_is:boolean(Boolean2_string),
	false = a_value_str_is:boolean(Boolean_wrong),
	{true,Boolean_digit1} = a_value_str_is:boolean_digit(Boolean_digit1_string),
	{true,Boolean_digit2} = a_value_str_is:boolean_digit(Boolean_digit2_string),
	false = a_value_str_is:boolean_digit(Boolean_wrong),
	io:format("DONE! Boolean values verification test passed.~n"),
	Latin_name = "Vasya Pukin",
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
	Email = "test@arboreus.systems",
	{true,Email} = email(Email),
	false = email(Latin_name),
	io:format("DONE! Email values verification test passed.~n"),
	Fqdn1 = "fqdn1765_098.test_domain.example.com.",
	Fqdn_wrong = "wrong_fqdn",
	{true,Fqdn1} = fqdn(Fqdn1),
	false = fqdn(Fqdn_wrong),
	io:format("DONE! FQDN values verification test passed.~n"),
	Term = [erlang,term],
	Term_string = "[erlang,term].",
	Term_wrong = "wrong_term",
	{true,Term} = term(Term_string),
	false = term(Term_wrong),
	io:format("DONE! Erlang Terms values verification test passed.~n"),
	IPv4_tuple = {1,1,1,1},
	IPv4_string = "1.1.1.1",
	IPv4_integer = 16843009,
	IPv4_wrong = "ip_wrong",
	{true,IPv4_tuple} = ipv4(IPv4_string,tuple),
	{true,IPv4_integer} = ipv4(IPv4_string,integer),
	false = ipv4(IPv4_wrong,integer),
	false = ipv4(IPv4_wrong,tuple),
	io:format("DONE! IPv4 values verification test passed.~n"),
	IPv6_tuple = {8193,3512,4515,2519,7988,35374,1952,30301},
	IPv6_string = "2001:0db8:11a3:09d7:1f34:8a2e:07a0:765d",
	IPv6_integer = 42540766416740939402060931394078537309,
	IPv6_wrong = "ip_wrong",
	{true,IPv6_tuple} = ipv6(IPv6_string,tuple),
	{true,IPv6_integer} = ipv6(IPv6_string,integer),
	false = ipv6(IPv6_wrong,integer),
	false = ipv6(IPv6_wrong,tuple),
	io:format("DONE! IPv6 values verification test passed.~n"),
	Numeric1 = "12345",
	Numeric_wrong = "numeric_wrong",
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
	Alphanumeric = "bNBZOekdc4r71r7C",
	Alphanumeric_wrong1 = "12345",
	Alphanumeric_wrong2 = "bNB Oekdc4r71r7C",
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
	Base64_decoded = "Base64_string",
	Base64_encoded = base64:encode_to_string(Base64_decoded),
	Base64_wrong = "Base64_wrong",
	Base64_encoded_size = length(Base64_encoded),
	Base64_decoded_size = length(Base64_decoded),
	{true,Base64_decoded} = base64(Base64_encoded,string),
	{true,Base64_encoded} = base64(Base64_encoded,base64),
	false = base64(Base64_wrong,string),
	{true,Base64_decoded} = base64_limited(Base64_encoded,string,{equal,Base64_decoded_size}),
	false = base64_limited(Base64_encoded,string,{equal,Base64_decoded_size+1}),
	{true,Base64_encoded} = base64_limited(Base64_encoded,base64,{equal,Base64_encoded_size}),
	false = base64_limited(Base64_encoded,string,{equal,Base64_encoded_size+1}),
	{true,Base64_decoded} = base64_limited(Base64_encoded,string,{less_or_equal,Base64_decoded_size}),
	false = base64_limited(Base64_encoded,string,{less_or_equal,Base64_decoded_size-1}),
	{true,Base64_encoded} = base64_limited(Base64_encoded,base64,{less_or_equal,Base64_encoded_size}),
	false = base64_limited(Base64_encoded,string,{less_or_equal,Base64_encoded_size-10}),
	{true,Base64_decoded} = base64_limited(Base64_encoded,string,{more_or_equal,Base64_decoded_size}),
	false = base64_limited(Base64_encoded,string,{more_or_equal,Base64_decoded_size+1}),
	{true,Base64_encoded} = base64_limited(Base64_encoded,base64,{more_or_equal,Base64_encoded_size}),
	false = base64_limited(Base64_encoded,string,{more_or_equal,Base64_encoded_size+1}),
	{true,Base64_decoded} = base64_limited(Base64_encoded,string,{ranged,Base64_decoded_size-1,Base64_decoded_size+1}),
	false = base64_limited(Base64_encoded,string,{ranged,Base64_decoded_size+1,Base64_decoded_size+2}),
	{true,Base64_encoded} = base64_limited(Base64_encoded,base64,{ranged,Base64_encoded_size-1,Base64_encoded_size+1}),
	false = base64_limited(Base64_encoded,string,{ranged,Base64_encoded_size+1,Base64_encoded_size+2}),
	io:format("DONE! Base64 values verification test passed.~n"),
	Password_decoded = "qwerty",
	Password_encoded = base64:encode_to_string(Password_decoded),
	Password_wrong = "password_wrong",
	{true,Password_decoded} = password(Password_encoded,4,8),
	false = password(Password_encoded,1,2),
	false = password(Password_wrong,4,8),
	io:format("DONE! Password values verification test passed.~n"),
	UTF_string = "utf_string",
	UTF_size = length(UTF_string),
	UTF_string_wrong = utf_wrong,
	{true,UTF_string} = utf_free(UTF_string),
	false = utf_free(UTF_string_wrong),
	{true,UTF_string} = utf_limited(UTF_string,{equal,UTF_size}),
	false = utf_limited(UTF_string,{equal,UTF_size-1}),
	{true,UTF_string} = utf_limited(UTF_string,{less_or_equal,UTF_size}),
	false = utf_limited(UTF_string,{less_or_equal,UTF_size-1}),
	{true,UTF_string} = utf_limited(UTF_string,{more_or_equal,UTF_size}),
	false = utf_limited(UTF_string,{more_or_equal,UTF_size+1}),
	{true,UTF_string} = utf_limited(UTF_string,{ranged,UTF_size-1,UTF_size+1}),
	false = utf_limited(UTF_string,{ranged,UTF_size+1,UTF_size+2}),
	{true,UTF_string} = utf_by_pattern(UTF_string,"^.{1,}$"),
	false = utf_by_pattern(UTF_string,"^[A]{1}$"),
	io:format("DONE! UTF string verification test passed.~n"),
	Time_tuple = erlang:localtime(),
	Time_tuple_string = a_term:to_utf_string(Time_tuple),
	Time_tuple_string_wrong = "wrong_time_tuple",
	{true,Time_tuple} = time_tuple(Time_tuple_string),
	false = time_tuple(Time_tuple_string_wrong),
	Time_timestamp = a_time:timestamp(),
	Time_timestamp_string = integer_to_list(Time_timestamp),
	Time_timestamp_wrong = <<("wrong_timestamp")/utf8>>,
	{true,Time_timestamp} = time_timestamp(Time_timestamp_string),
	false = time_timestamp(Time_timestamp_wrong),
	Time_ansi = a_time:format(ansi,{date_tuple,Time_tuple}),
	Time_rfc850 = a_time:format(rfc850,{date_tuple,Time_tuple}),
	Time_rfc822 = a_time:format(rfc822,{date_tuple,Time_tuple}),
	{true,Time_tuple} = time_ansi(Time_ansi,tuple),
	false = time_ansi(Time_tuple_string_wrong,tuple),
	{true,Time_tuple} = time_rfc850(Time_rfc850,tuple),
	false = time_rfc850(Time_tuple_string_wrong,tuple),
	{true,Time_tuple} = time_rfc822(Time_rfc822,tuple),
	false = time_rfc822(Time_tuple_string_wrong,tuple),
	io:format("DONE! Time verification test passed.~n"),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_value_str_is) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Verify time-tuple string value
-spec time_tuple(Utf_string) -> {true,a_time_tuple()} | false
	when
	Utf_string :: utf_text_string().

time_tuple(Binary) ->
	try
		{{Year,Month,Day},{Hours,Minutes,Seconds}} = a_term:from_utf_string(Binary),
		{true,{{Year,Month,Day},{Hours,Minutes,Seconds}}}
	catch
		_:_  -> false
	end.


%% ----------------------------
%% @doc Verify UNIX-timestamp string value
-spec time_timestamp(Utf_string) -> {true,a_time_unix_timestamp()} | false
	when
	Utf_string :: utf_text_binary().

time_timestamp(Utf_string) -> integer_pos(Utf_string).


%% ----------------------------
%% @doc Verify rfc822 unicode time string value
-spec time_rfc822(Utf_string,Return_mode) -> {true,Time} | false
	when
	Utf_string :: utf_text_string(),
	Return_mode :: tuple | seconds | timestamp,
	Time :: pos_integer() | a_time_tuple().

time_rfc822(Utf_string,Return_mode) ->
	case a_time:from_formated(rfc822,Utf_string,Return_mode) of
		{error,_} -> false;
		false -> false;
		Result -> {true,Result}
	end.


%% ----------------------------
%% @doc Verify rfc850 unicode time string value
-spec time_rfc850(Utf_string,Return_mode) -> {true,Time} | false
	when
	Utf_string :: utf_text_string(),
	Return_mode :: tuple | seconds | timestamp,
	Time :: pos_integer() | a_time_tuple().

time_rfc850(Utf_string,Return_mode) ->
	case a_time:from_formated(rfc850,Utf_string,Return_mode) of
		{error,_} -> false;
		false -> false;
		Result -> {true,Result}
	end.


%% ----------------------------
%% @doc Verify ANSI unicode time string value
-spec time_ansi(Utf_string,Return_mode) -> {true,Time} | false
	when
	Utf_string :: utf_text_string(),
	Return_mode :: tuple | seconds | timestamp,
	Time :: pos_integer() | a_time_tuple().

time_ansi(Utf_string,Return_mode) ->
	case a_time:from_formated(ansi,Utf_string,Return_mode) of
		{error,_} -> false;
		false -> false;
		Result -> {true,Result}
	end.


%% ----------------------------
%% @doc Verify unicode string by pattern
-spec utf_by_pattern(Utf_string,Pattern) -> {true,utf_text_string()} | false
	when
	Utf_string :: utf_text_string(),
	Pattern :: utf_text_string().

utf_by_pattern(Utf_string,Pattern) ->
	case utf_free(Utf_string) of
		{true,Utf_string} -> by_pattern(Utf_string,Pattern);
		Result -> Result
	end.


%% ----------------------------
%% @doc Verify limited unicode string
-spec utf_limited(Utf_string,Limit) -> {true,utf_text_string()} | false
	when
	Utf_string :: utf_text_string(),
	Limit :: {equal,Length} | {less_or_equal,Length} | {more_or_equal,Length} | {ranged,Minimal,Maximal},
	Length :: pos_integer(),
	Minimal :: pos_integer(),
	Maximal :: pos_integer().

utf_limited(Utf_string,Limit) ->
	case utf_free(Utf_string) of
		{true,Binary} -> by_size(Binary,Limit);
		Result -> Result
	end.


%% ----------------------------
%% @doc Verify unicode string
-spec utf_free(Utf_string) -> {true,utf_text_string()} | false
	when
	Utf_string :: utf_text_string().

utf_free(Utf_string) when is_list(Utf_string) ->
	case io_lib:printable_unicode_list(Utf_string) of
		true -> {true,Utf_string};
		_ -> false
	end;
utf_free(_) -> false.


%% ----------------------------
%% @doc Verify password string value
-spec password(Utf_string,Minimal_length,Maximal_length) -> {true,utf_text_string()} | false
	when
	Utf_string :: utf_text_string(),
	Minimal_length :: pos_integer(),
	Maximal_length :: pos_integer().

password(Binary,Minimal,Maximal) ->
	base64_limited(Binary,string,{ranged,Minimal,Maximal}).


%% ----------------------------
%% @doc Verify Base64 string limited value
-spec base64_limited(Utf_string,Return_mode,Limit) -> {true,Result} | false
	when
	Utf_string :: utf_text_string(),
	Return_mode :: base64 | string,
	Limit :: {equal,Length} | {less_or_equal,Length} | {more_or_equal,Length} | {ranged,Minimal,Maximal},
	Length :: pos_integer(),
	Minimal :: pos_integer(),
	Maximal :: pos_integer(),
	Result :: utf_base64_string() | utf_text_string().

base64_limited(Utf_string,Return_mode,Limit) ->
	case Return_mode of
		string ->
			case base64(Utf_string,Return_mode) of
				{true,Encoded_string} -> by_size(Encoded_string,Limit);
				Result -> Result
			end;
		_ ->
			case by_size(Utf_string,Limit) of
				{true,Utf_string} -> base64(Utf_string,base64);
				Result -> Result
			end
	end.


%% ----------------------------
%% @doc Verify Base64 string value
-spec base64(Utf_string,Return_mode) -> {true,Result} | false
	when
	Utf_string :: utf_base64_string(),
	Return_mode :: base64 | string,
	Result :: utf_base64_string() | utf_text_string().

base64(Utf_string,Return_mode) ->
	try
		Encoded_string = base64:decode_to_string(Utf_string),
		case Return_mode of
			string -> {true,Encoded_string};
			_ -> {true,Utf_string}
		end
	catch _:_ -> false end.


%% ----------------------------
%% @doc Verify alphanumeric ID string value
-spec id_alphanumeric(Utf_string,Length) -> {true,utf_text_string()} | false
	when
	Utf_string :: utf_text_string(),
	Length :: pos_integer().

id_alphanumeric(Utf_string,Length) -> alphanumeric_limited(Utf_string,{equal,Length}).


%% ----------------------------
%% @doc Verify numeric ID string value
-spec id_numeric(Utf_string,Length) -> {true,utf_text_string()} | false
	when
	Utf_string :: utf_text_string(),
	Length :: pos_integer().

id_numeric(Utf_string,Length) -> numeric_limited(Utf_string,{equal,Length}).


%% ----------------------------
%% @doc Verify limited alphanumeric string value
-spec alphanumeric_limited(Utf_string,Limit) -> {true,utf_text_string()} | false
	when
	Utf_string :: utf_text_string(),
	Limit :: {equal,Length} | {less_or_equal,Length} | {more_or_equal,Length} | {ranged,Minimal,Maximal},
	Length :: pos_integer(),
	Minimal :: pos_integer(),
	Maximal :: pos_integer().

alphanumeric_limited(Utf_string,Limit) ->
	case by_size(Utf_string,Limit) of
		{true,Utf_string} -> alphanumeric(Utf_string);
		Result -> Result
	end.


%% ----------------------------
%% @doc Verify alphanumeric string value
-spec alphanumeric(Utf_string) -> {true,utf_text_string()} | false
	when
	Utf_string :: utf_text_string().

alphanumeric(Utf_string) -> by_pattern(Utf_string,"^[a-zA-Z0-9]{1,}$").


%% ----------------------------
%% @doc Verify limited numeric string value
-spec numeric_limited(Utf_string,Limit) -> {true,utf_text_string()} | false
	when
	Utf_string :: utf_text_string(),
	Limit :: {equal,Length} | {less_or_equal,Length} | {more_or_equal,Length} | {ranged,Minimal,Maximal},
	Length :: pos_integer(),
	Minimal :: pos_integer(),
	Maximal :: pos_integer().

numeric_limited(Utf_string,Limit) ->
	case by_size(Utf_string,Limit) of
		{true,Utf_string} -> numeric(Utf_string);
		_ -> false
	end.


%% ----------------------------
%% @doc Verify numeric string value
-spec numeric(Utf_string) -> {true,utf_text_string()} | false
	when
	Utf_string :: utf_text_string().

numeric(Utf_string) -> by_pattern(Utf_string,"^[0-9]{1,}$").


%% ----------------------------
%% @doc Verify IPv6 string value
-spec ipv6(Utf_string,Return_mode) -> {true,Ip} | false
	when
	Utf_string :: utf_text_string(),
	Return_mode :: integer | tuple,
	Ip :: ipv6_tuple() | ipv4_integer().

ipv6(Utf_string,Return_mode) ->
	try
		case Return_mode of
			integer ->
				{true,a_net:ipv6_to_integer(Utf_string)};
			_ ->
				{ok,Ip_tuple} = inet:parse_ipv6_address(Utf_string),
				{true,Ip_tuple}
		end
	catch
		_:_ -> false
	end.


%% ----------------------------
%% @doc Verify IPv4 string value
-spec ipv4(Utf_string,Return_mode) -> {true,Ip} | false
	when
	Utf_string :: utf_text_string(),
	Return_mode :: integer | tuple,
	Ip :: ipv4_integer() | ipv4_tuple().

ipv4(Utf_string,Return_mode) ->
	try
		{ok,Ip_tuple} = inet:parse_ipv4_address(Utf_string),
		case Return_mode of
			integer -> {true,a_net:ipv4_to_integer(Ip_tuple)};
			_ -> {true,Ip_tuple}
		end
	catch
		_:_ -> false
	end.


%% ----------------------------
%% @doc Verify Erlang term string value
-spec term(Utf_string) -> {true,term()} | false
	when
	Utf_string :: utf_text_string().

term(Utf_string) ->
	try {true,a_term:from_utf_string(Utf_string)}
	catch _:_ -> false end.


%% ----------------------------
%% @doc Verify FQND string value
-spec fqdn(Utf_string) -> {true,utf_text_string()} | false
	when
	Utf_string :: utf_text_string().

fqdn(Utf_string) ->
	case by_size(Utf_string,{less_or_equal,255}) of
		{true,Utf_string} -> by_pattern(Utf_string,"^([a-z0-9\-\ \_]{1,}[\.]{1}){1,}$");
		Result -> Result
	end.


%% ----------------------------
%% @doc Verify email string by pattern
-spec email(Utf_string) -> {true,utf_text_string()} | false
	when
	Utf_string :: utf_text_string().

email(Utf_string) ->
	by_pattern(Utf_string,"^([a-z0-9\.\_\-]{1,})\@([a-z0-9\.\_\-]{1,})$").


%% ----------------------------
%% @doc Verify latin name string value limited by length
-spec latin_name_limited(Utf_string,Limit) -> {true,utf_text_string()} | false
	when
	Utf_string :: utf_text_string(),
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
%% @doc Verify latin name string by pattern
-spec latin_name(Utf_string) -> {true,utf_text_string()} | false
	when
	Utf_string :: utf_text_string().

latin_name(Utf_string) ->
	by_pattern(Utf_string,"^(\ ?[A-Z]{1}[a-z]{0,}){1,}$").


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
