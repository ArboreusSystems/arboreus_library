%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV (http://alexandr.kirilov.me)
%%% @copyright (C) 2015, Arboreus, (http://arboreus.systems)
%%% @doc The HTTP POST parameters vaidation handler
%%%
%%% @end
%%% Created : 06. Sep 2015 0:18
%%%-------------------------------------------------------------------
-module(a_yaws_params).
-author("Alexandr KIRILOV (http://alexandr.kirilov.me)").

%% Application includes
-include("a_includes.hrl").

%% Module API
-export([

	test/0,

	not_required_check/4,not_required_check/5,
	not_required_schema_check/2,

	required_check/3,
	required_checkout/4,
	required_check_list/2,
	required_check_parameters/2

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->

	0.2 = required_check(float,"0.2",[]),
	nomatch = required_check(float,"aaa",[]),
	nomatch = required_check(float,"1",[]),

	0.2 = required_check(float_positive,"0.2",[]),
	nomatch = required_check(float_positive,"-0.2",[]),
	nomatch = required_check(float_positive,"aaa",[]),
	nomatch = required_check(float_positive,"1",[]),

	-0.2 = required_check(float_negative,"-0.2",[]),
	nomatch = required_check(float_negative,"0.2",[]),
	nomatch = required_check(float_negative,"aaa",[]),
	nomatch = required_check(float_negative,"1",[]),

	-0.2 = required_check(float_from_list,"-0.2",[[-0.2]]),
	nomatch = required_check(float_from_list,"-0.2",[[0.2]]),
	nomatch = required_check(float_from_list,"aaa",[[0.2]]),
	nomatch = required_check(float_from_list,"1",[[0.2]]),

	-1.2 = required_check(float_ranged,"-1.2",[-2.2,3.5]),
	nomatch = required_check(float_ranged,"-2.5",[-2.2,3.5]),
	nomatch = required_check(float_ranged,"-4.2",[-2.2,3.5]),
	nomatch = required_check(float_ranged,"aaa",[-2.2,3.5]),
	nomatch = required_check(float_ranged,"1",[-2.2,3.5]),

	10 = required_check(integer,"10",[]),
	nomatch = required_check(integer,"aaa",[]),
	nomatch = required_check(integer,"0.1",[]),

	10 = required_check(integer_positive,"10",[]),
	nomatch = required_check(integer_positive,"10.1",[]),
	nomatch = required_check(integer_positive,"-10",[]),
	nomatch = required_check(integer_positive,"aaa",[]),

	-10 = required_check(integer_negative,"-10",[]),
	nomatch = required_check(integer_negative,"10.1",[]),
	nomatch = required_check(integer_negative,"10",[]),
	nomatch = required_check(integer_negative,"aaa",[]),

	1 = required_check(integer_from_list,"1",[[1]]),
	nomatch = required_check(integer_from_list,"1",[[2]]),
	nomatch = required_check(integer_from_list,"-0.2",[[1]]),
	nomatch = required_check(integer_from_list,"aaa",[[1]]),

	0 = required_check(integer_ranged,"0",[-2,5]),
	nomatch = required_check(integer_ranged,"10",[-2,5]),
	nomatch = required_check(integer_ranged,"4.2",[-2,5]),
	nomatch = required_check(integer_ranged,"aaa",[-2,5]),
	nomatch = required_check(integer_ranged,"-10",[-2,5]),

	atom = required_check(atom,"atom",[]),
	'ATOM' = required_check(atom,"ATOM",[]),
	'1' = required_check(atom,"1",[]),
	'1.0' = required_check(atom,"1.0",[]),

	true = required_check(boolean,"true",[]),
	true = required_check(boolean,"1",[]),
	false = required_check(boolean,"false",[]),
	false = required_check(boolean,"0",[]),
	nomatch = required_check(boolean,"11",[]),

	1 = required_check(boolean_integer,"true",[]),
	1 = required_check(boolean_integer,"1",[]),
	0 = required_check(boolean_integer,"false",[]),
	0 = required_check(boolean_integer,"0",[]),
	nomatch = required_check(boolean_integer,"11",[]),

	"name" = required_check(latin_name,"name",[4,string]),
	<<"name">> = required_check(latin_name,"name",[4,binary]),
	<<"name">> = required_check(latin_name,"name",[5,binary]),
	nomatch = required_check(latin_name,"name",[2,binary]),

	"name" = required_check(latin_name_ranged,"name",[2,6,string]),
	<<"name">> = required_check(latin_name_ranged,"name",[4,4,binary]),
	nomatch = required_check(latin_name_ranged,"name",[5,9,binary]),

	<<"dGVzdA==">> = required_check(base64,"dGVzdA==",[binary]),
	"dGVzdA==" = required_check(base64,"dGVzdA==",[string]),
	nomatch = required_check(base64,"wrong_string",[string]),

	<<"test">> = required_check(base64_encoded,"dGVzdA==",[binary]),
	"test" = required_check(base64_encoded,"dGVzdA==",[string]),
	nomatch = required_check(base64_encoded,"wrong_string",[string]),

	<<"testID">> = required_check(id,"testID",[6,binary]),
	"testID" = required_check(id,"testID",[6,string]),
	nomatch = required_check(id,"testID",[5,binary]),
	nomatch = required_check(id,"testID",[7,binary]),

	<<"testID">> = required_check(id_or_null,"testID",[6,"0",binary]),
	null = required_check(id_or_null,"0",[6,"0",binary]),
	nomatch = required_check(id_or_null,"wrong_id",[6,"0",binary]),
	nomatch = required_check(id_or_null,"wrongidwrongid",[6,"0",binary]),

	<<"testID">> = required_check(id_ranged,"testID",[4,7,binary]),
	"testID" = required_check(id_ranged,"testID",[4,7,string]),
	nomatch = required_check(id_ranged,"testID",[7,10,binary]),
	nomatch = required_check(id_ranged,"test ID",[4,10,string]),

	<<"testID">> = required_check(id_ranged_or_null,"testID",[4,7,"0",binary]),
	null = required_check(id_ranged_or_null,"0",[4,7,"0",binary]),
	nomatch = required_check(id_ranged_or_null,"0000000000000",[4,7,"0",binary]),

	<<"79054025255fb1a26e4bc422aef54eb4">> = required_check(id_md5,"79054025255fb1a26e4bc422aef54eb4",[binary]),
	"79054025255fb1a26e4bc422aef54eb4" = required_check(id_md5,"79054025255fb1a26e4bc422aef54eb4",[string]),
	nomatch = required_check(id_md5,"79054025255fb1a26e4bc422aef54",[string]),

	ok.


%% ----------------------------
%% @doc Check not required parameter
-spec not_required_check(PARAMETER_NAME,TYPE,TYPE_PROPERTIES,POST_PARAMETERS) ->
	CHECKED_VALUE | default
	when
		PARAMETER_NAME :: a_utf_text_string(),
		TYPE :: atom(),
		TYPE_PROPERTIES :: a_list_of_properties(),
		POST_PARAMETERS :: a_yaws_post_parameters(),
		CHECKED_VALUE :: any().

not_required_check(PARAMETER_NAME,TYPE,TYPE_PROPERTIES,POST_PARAMETERS) ->

	not_required_check(PARAMETER_NAME,TYPE,TYPE_PROPERTIES,default,POST_PARAMETERS).


%% ----------------------------
%% @doc Check not required parameter or return default value
-spec not_required_check(PARAMETER_NAME,TYPE,TYPE_PROPERTIES,DEFAULT,POST_PARAMETERS) ->
	CHECKED_VALUE | DEFAULT
	when
		PARAMETER_NAME :: a_utf_text_string(),
		TYPE :: atom(),
		TYPE_PROPERTIES :: a_list_of_properties(),
		POST_PARAMETERS :: a_yaws_post_parameters(),
		CHECKED_VALUE :: any(),
		DEFAULT :: any().

not_required_check(PARAMETER_NAME,TYPE,TYPE_PROPERTIES,DEFAULT,POST_PARAMETERS) ->

	case proplists:get_value(PARAMETER_NAME,POST_PARAMETERS) of
		undefined -> DEFAULT;
		VALUE_STRING -> required_check(TYPE,VALUE_STRING,TYPE_PROPERTIES)
	end.


%% ----------------------------
%% @doc Check not required parameters schema
-spec not_required_schema_check(SCHEMA,POST_PARAMETERS) ->
	CHECKED_PARAMETERS | {nomatch,NAME}
	when
		SCHEMA :: [{NAME,TYPE,TYPE_PROPERTIES}],
		NAME :: a_utf_text_string(),
		TYPE :: atom(),
		TYPE_PROPERTIES :: a_list_of_properties(),
		POST_PARAMETERS :: a_yaws_post_parameters(),
		CHECKED_PARAMETERS :: proplists:proplist().

not_required_schema_check(SCHEMA,POST_PARAMETERS) ->

	not_required_schema_check(SCHEMA,POST_PARAMETERS,[]).


%% ----------------------------
%% @doc Handler for not_required_schema_check/2
-spec not_required_schema_check(SCHEMA,POST_PARAMETERS,CHECKED_PARAMETERS) ->
	CHECKED_PARAMETERS | {nomatch,NAME}
	when
		SCHEMA :: [{NAME,TYPE,TYPE_PROPERTIES}],
		NAME :: a_utf_text_string(),
		TYPE :: atom(),
		TYPE_PROPERTIES :: a_list_of_properties(),
		POST_PARAMETERS :: a_yaws_post_parameters(),
		CHECKED_PARAMETERS :: proplists:proplist().

not_required_schema_check([],_POST_PARAMETERS,OUTPUT) -> OUTPUT;

not_required_schema_check([{NAME,TYPE,TYPE_PROPERTIES}|SCHEMA],POST_PARAMETERS,OUTPUT) ->

	case not_required_check(NAME,TYPE,TYPE_PROPERTIES,POST_PARAMETERS) of
		default -> not_required_schema_check(SCHEMA,POST_PARAMETERS,OUTPUT);
		nomatch -> {nomatch,NAME};
		CHECKED_VALUE -> not_required_schema_check(SCHEMA,POST_PARAMETERS,[{NAME,CHECKED_VALUE}|OUTPUT])
	end.


%% ----------------------------
%% @doc Checking the request parameter through the Regexp defined for the type.
-spec required_check(TYPE,PARAMETER,TYPE_PROPERTIES) -> nomatch | CHECKED_VALUE
	when
		TYPE :: atom(),
		PARAMETER :: a_http_post_parameter(),
		TYPE_PROPERTIES :: a_list_of_properties(),
		CHECKED_VALUE :: any().

required_check(user_defined,PARAMETER,[MODULE,FUNCTION,ARGUMENTS]) ->

	apply(MODULE,FUNCTION,[PARAMETER,ARGUMENTS]);

required_check(TYPE,PARAMETER,TYPE_PROPERTIES) ->

	parameter_value(TYPE,PARAMETER,TYPE_PROPERTIES).


%% ----------------------------
%% @doc Find and check parameter from Yaws parameters proplist
-spec required_checkout(PARAMETER_NAME,PARAMETERS,TYPE,TYPE_PROPERTIES) ->
	notinlist | nomatch | CHECKED_VALUE
	when
		PARAMETER_NAME :: string(),
		PARAMETERS :: proplists:proplist(),
		TYPE :: atom(),
		TYPE_PROPERTIES :: list(),
		CHECKED_VALUE :: any().

required_checkout(PARAMETER_NAME,PARAMETERS,TYPE,TYPE_PROPERTIES) ->

	case proplists:get_value(PARAMETER_NAME,PARAMETERS) of
		undefined -> notinlist;
		VALUE_STRING -> required_check(TYPE,VALUE_STRING,TYPE_PROPERTIES)
	end.


%% ----------------------------
%% @doc Wrapper function for check/3, checking list of typed elements
-spec required_check_list(LIST,TYPE_PROPERTIES) -> list() | nomatch
	when
		LIST :: list(),
		TYPE_PROPERTIES :: {TYPE,TYPE_PARAMETERS},
		TYPE :: atom(),
		TYPE_PARAMETERS :: list().

required_check_list(LIST,TYPE_PROPERTIES) -> required_check_list(LIST,TYPE_PROPERTIES,[]).


%% ----------------------------
%% @doc Checking list of typed elements
-spec required_check_list(LIST,TYPE_PROPERTIES,OUTPUT) -> list() | nomatch
	when
		LIST :: list(),
		TYPE_PROPERTIES :: {TYPE,TYPE_PARAMETERS},
		TYPE :: atom(),
		TYPE_PARAMETERS :: list(),
		OUTPUT :: list().

required_check_list([],_,OUTPUT) -> OUTPUT;

required_check_list([ELEMENT|LIST],{TYPE,TYPE_PARAMETERS},OUTPUT) ->

	case required_check(TYPE,ELEMENT,TYPE_PARAMETERS) of
		nomatch -> nomatch;
		CHECKED_ELEMENT ->
			required_check_list(
				LIST,
				{TYPE,TYPE_PARAMETERS},
				lists:append(OUTPUT,[CHECKED_ELEMENT])
			)
	end.


%% ----------------------------
%% @doc Checking requested parameters in following of Data_schema
-spec required_check_parameters(DATA_SCHEMA,PARAMETERS) -> CHECKED_PARAMETERS | false
	when
		DATA_SCHEMA :: list(),
		PARAMETERS :: list(),
		CHECKED_PARAMETERS :: proplists:proplist().

required_check_parameters(DATA_SCHEMA,PARAMETERS) ->

	required_check_parameters(DATA_SCHEMA,PARAMETERS,[]).


%% ----------------------------
%% @doc Handler function for check_parameters/2
-spec required_check_parameters([RULE|DATA_SCHEMA],PARAMETERS,RESULT) -> CHECKED_PARAMETERS | false
	when
		RULE :: {PARAMETER_NAME,PARAMETER_PROPERTIES},
		PARAMETER_NAME :: a_utf_text_string(),
		PARAMETER_PROPERTIES :: proplists:proplist(),
		DATA_SCHEMA :: proplists:proplist(),
		PARAMETERS :: list(),
		RESULT :: CHECKED_PARAMETERS | false,
		CHECKED_PARAMETERS :: proplists:proplist().

required_check_parameters([],_,RESULT) -> RESULT;

required_check_parameters([RULE|DATA_SCHEMA],PARAMETERS,RESULT) ->

	{PARAMETER_NAME,PARAMETER_PROPERTIES} = RULE,

	CHECK = fun(F_PARAMETER_VALUE) ->
		TYPE = proplists:get_value(type,PARAMETER_PROPERTIES),
		TYPE_PROPERTIES = proplists:get_value(type_properties,PARAMETER_PROPERTIES),
		PARAMETER_CHECKED = required_check(TYPE,F_PARAMETER_VALUE,TYPE_PROPERTIES),
		case PARAMETER_CHECKED of
			nomatch ->
				false;
			PARAMETER_CHECKED ->
				NAME = list_to_atom(PARAMETER_NAME),
				F_OUTPUT = lists:append(RESULT,[{NAME,PARAMETER_CHECKED}]),
				required_check_parameters(DATA_SCHEMA,PARAMETERS,F_OUTPUT)
		end
	end,

	case proplists:get_value(require,PARAMETER_PROPERTIES) of
		true ->
			case proplists:get_value(PARAMETER_NAME,PARAMETERS) of
				undefined -> false;
				PARAMETER_VALUE -> CHECK(PARAMETER_VALUE)
			end;
		false ->
			case proplists:get_value(PARAMETER_NAME,PARAMETERS) of
				undefined -> CHECK(proplists:get_value(require_default,PARAMETER_PROPERTIES));
				PARAMETER_VALUE -> CHECK(PARAMETER_VALUE)
			end
	end.


%%-----------------------------------
%% @doc Secondary function for check/3
-spec parameter_value(TYPE,PARAMETER,TYPE_PROPERTIES) ->
	nomatch | CHECKED_VALUE | {error,REASON}
	when
		TYPE :: atom(),
		PARAMETER :: a_http_post_parameter(),
		TYPE_PROPERTIES ::list(),
		CHECKED_VALUE :: any(),
		REASON :: term().

%% List of typed elements
parameter_value(list_of_typed,PARAMETERS,[{SEPARATOR,TYPE,TYPE_PROPERTIES}]) ->

	required_check_list(
		string:tokens(PARAMETERS,SEPARATOR),
		{TYPE,TYPE_PROPERTIES}
	);

%% Float, regex rule ^[\-]?[0-9]*\.[0-9]*$
parameter_value(float,PARAMETER,_) ->

	a_yaws_params_primitives:float(PARAMETER);

%% Float, regex rule ^[0-9]*\.[0-9]*$
parameter_value(float_positive,PARAMETER,_) ->

	a_yaws_params_primitives:float_positive(PARAMETER);

%% Negative float
parameter_value(float_negative,PARAMETER,_) ->

	a_yaws_params_primitives:float_negative(PARAMETER);

%% Float from list
parameter_value(float_from_list,PARAMETER,[LIST]) ->

	a_yaws_params_primitives:float_from_list(PARAMETER,LIST);

%% Ranged float
parameter_value(float_ranged,PARAMETER,[MINOR,MAJOR]) ->

	a_yaws_params_primitives:float_ranged(PARAMETER,MINOR,MAJOR);

%% Integer, regex rule ^[\-]?[0-9]*$
parameter_value(integer,PARAMETER,_) ->

	a_yaws_params_primitives:integer(PARAMETER);

%% Positive integer, regex rule "^[0-9]*$"
parameter_value(integer_positive,PARAMETER,_) ->

	a_yaws_params_primitives:integer_positive(PARAMETER);

%% Neg_integer, regex rule ^[\-]{1}[0-9]*$
parameter_value(integer_negative,PARAMETER,_) ->

	a_yaws_params_primitives:integer_negative(PARAMETER);

%% Integer from list
parameter_value(integer_from_list,PARAMETER,[LIST]) ->

	a_yaws_params_primitives:integer_from_list(PARAMETER,LIST);

%% Ranged integer
parameter_value(integer_ranged,PARAMETER,[MINOR,MAJOR]) ->

	a_yaws_params_primitives:integer_ranged(PARAMETER,MINOR,MAJOR);

%% Atom
parameter_value(atom,PARAMETER,_) ->

	a_yaws_params_primitives:atom(PARAMETER);

%% Atom from list
parameter_value(atom_from_list,PARAMETER,[LIST]) ->

	a_yaws_params_primitives:atom_from_list(PARAMETER,LIST);

%% Boolean
parameter_value(boolean,PARAMETER,_) ->

	a_yaws_params_primitives:boolean(PARAMETER);

%% Boolean integer
parameter_value(boolean_integer,PARAMETER,_) ->

	a_yaws_params_primitives:boolean_integer(PARAMETER);

%% Latin_name, regex rule ^[a-zA-Z]{1}[a-zA-Z0-9\_\-]{1,LENGTH}$
parameter_value(latin_name,PARAMETER,[LENGTH,OUTPUT_TYPE]) ->

	a_yaws_params_primitives:latin_name(PARAMETER,LENGTH,OUTPUT_TYPE);

%% Latin_name_ranged, regex rule ^[a-zA-Z]{1}[a-zA-Z0-9\_\-]{MINOR,MAJOR}$
parameter_value(latin_name_ranged,PARAMETER,[MINOR,MAJOR,OUTPUT_TYPE]) ->

	a_yaws_params_primitives:latin_name_ranged(PARAMETER,MINOR,MAJOR,OUTPUT_TYPE);

%% Unicode_base64 ^([a-zA-Z0-9\=\+\/]{0,})$
parameter_value(base64,PARAMETER,[OUTPUT_TYPE]) ->

	a_yaws_params_primitives:base64(PARAMETER,OUTPUT_TYPE);

%% Unicode_base64 ^([a-zA-Z0-9\=\+\/]{0,})$
parameter_value(base64_encoded,PARAMETER,[OUTPUT_TYPE]) ->

	a_yaws_params_primitives:base64_encoded(PARAMETER,OUTPUT_TYPE);

%% Id
parameter_value(id,PARAMETER,[LENGTH,OUTPUT_TYPE]) ->

	a_yaws_params_primitives:id(PARAMETER,LENGTH,OUTPUT_TYPE);

%% Id or null
parameter_value(id_or_null,PARAMETER,[LENGTH,NULL,OUTPUT_NULL,OUTPUT_TYPE]) ->

	a_yaws_params_primitives:id_or_null(PARAMETER,LENGTH,NULL,OUTPUT_NULL,OUTPUT_TYPE);

%% Id ranged
parameter_value(id_ranged,PARAMETER,[MINOR,MAJOR,OUTPUT_TYPE]) ->

	a_yaws_params_primitives:id_ranged(PARAMETER,MINOR,MAJOR,OUTPUT_TYPE);

%% Id ranged or null
parameter_value(id_ranged_or_null,PARAMETER,[MINOR,MAJOR,NULL,OUTPUT_TYPE]) ->

	a_yaws_params_primitives:id_ranged_or_null(PARAMETER,MINOR,MAJOR,NULL,OUTPUT_TYPE);

%% Id of md5
parameter_value(id_md5,PARAMETER,[OUTPUT_TYPE]) ->

	a_yaws_params_primitives:id_md5(PARAMETER,OUTPUT_TYPE);

%% Id of md4
parameter_value(id_md4,PARAMETER,[OUTPUT_TYPE]) ->

	a_yaws_params_primitives:id_md4(PARAMETER,OUTPUT_TYPE);

%% IP v4
parameter_value(ipv4,PARAMETER,[OUTPUT_TYPE]) ->

	a_yaws_params_primitives:ip_v4(PARAMETER,OUTPUT_TYPE);

%% IP v4 range
parameter_value(ipv4_range,PARAMETER,[OUTPUT_TYPE]) ->

	a_yaws_params_primitives:ip_v4_range(PARAMETER,OUTPUT_TYPE);

%% IP v6
parameter_value(ipv6,PARAMETER,[OUTPUT_TYPE]) ->

	a_yaws_params_primitives:ip_v6(PARAMETER,OUTPUT_TYPE);

%% FQDN regex rule "^(\.?([a-zA-Z0-9\-\_]{1,})){0,}$"
parameter_value(fqdn,PARAMETER,[OUTPUT_TYPE]) ->

	a_yaws_params_primitives:fqdn(PARAMETER,OUTPUT_TYPE);

%% E-mail regex rule ^([a-zA-Z0-9\.\_\-]{1,})\@([a-zA-Z0-9\.\_\-]{1,})$
parameter_value(e_mail,PARAMETER,[OUTPUT_TYPE]) ->

	a_yaws_params_primitives:fqdn(PARAMETER,OUTPUT_TYPE);

%% Numerical sequece ^([0-9]{14,})$
parameter_value(numerical,PARAMETER,[LENGTH_RULE,OUTPUT_TYPE]) ->

	a_yaws_params_primitives:numerical(PARAMETER,LENGTH_RULE,OUTPUT_TYPE);

%% Password regex rule ^((?![t1j]).){1,}$
parameter_value(password,PARAMETER,[more_equal,LENGTH]) ->

	a_yaws_params_primitives:password(PARAMETER,LENGTH);

parameter_value(password,PARAMETER,[ranged,MINOR,MAJOR]) ->

	a_yaws_params_primitives:password_ranged(PARAMETER,MINOR,MAJOR);

%% Unicode binary, regex rule ^((?![t1j]).){1,}$
parameter_value(unicode_binary,PARAMETER,[free]) ->

	a_yaws_params_primitives:utf_binary(PARAMETER);

parameter_value(unicode_binary,PARAMETER,[free,free]) ->

	a_yaws_params_primitives:utf_binary(PARAMETER);

parameter_value(unicode_binary,PARAMETER,[free,{more_equal,LENGTH}]) ->

	a_yaws_params_primitives:utf_binary_limited(PARAMETER,{more_equal,LENGTH});

parameter_value(unicode_binary,PARAMETER,[free,{less_equal,LENGTH}]) ->

	a_yaws_params_primitives:utf_binary_limited(PARAMETER,{less_equal,LENGTH});

parameter_value(unicode_binary,PARAMETER,[free,{size,LENGTH}]) ->

	a_yaws_params_primitives:utf_binary_limited(PARAMETER,{less_equal,LENGTH});

parameter_value(unicode_binary,PARAMETER,[free,{range,MINOR,MAJOR}]) ->

	a_yaws_params_primitives:utf_binary_ranged(PARAMETER,MINOR,MAJOR);

parameter_value(unicode_binary,PARAMETER,[{except,EXCEPTION_CHARS},LENGTH_TYPE]) ->

	a_yaws_params_primitives:utf_binary_except(PARAMETER,EXCEPTION_CHARS,LENGTH_TYPE);

%% Formatted time checking
parameter_value(time,PARAMETER,[FORMAT,OUTPUT_TYPE]) ->

	a_yaws_params_primitives:time(PARAMETER,FORMAT,OUTPUT_TYPE);

%% Range positive integer, regex rule ^([0-9]{1,})\:([0-9]{1,})$
%% Range negative integer, regex rule ^(\-[0-9]{1,}|0)\:(\-[0-9]{1,}|0)$
%% Range integer, regex rule ^(\-?[0-9]{1,})\:(\-?[0-9]{1,})$
parameter_value(TYPE,PARAMETER,_)
	when
		TYPE == range_pos_integer;
		TYPE == range_neg_integer;
		TYPE == range_integer ->

	a_yaws_params_primitives:integer_range(PARAMETER,TYPE);

%% Limited range integer
parameter_value(range_integer_limited,PARAMETER,[{MINOR_A,MAJOR_A},{MINOR_B,MAJOR_B}])
	when
		is_integer(MINOR_A), is_integer(MAJOR_A),
		is_integer(MINOR_B), is_integer(MAJOR_B) ->

	a_yaws_params_primitives:integer_range_limited(PARAMETER,{MINOR_A,MAJOR_A},{MINOR_B,MAJOR_B});

%% Range positive float, regex rule ^([0-9]*\.[0-9]*)\:([0-9]*\.[0-9]*)$
%% Range negative float, regex rule ^(\-[0-9]{1,}\.[0-9]{1,}|0)\:(\-[0-9]{1,}\.[0-9]{1,}|0)$
%% Range float, regex rule ^(\-?[0-9]{1,}\.[0-9]{1,})\:(\-?[0-9]{1,}\.[0-9]{1,})$
parameter_value(TYPE,PARAMETER,_)
	when
		TYPE == range_pos_float;
		TYPE == range_neg_float;
		TYPE == range_float	->

	a_yaws_params_primitives:float_range(PARAMETER,TYPE);

%% Limited range integer
parameter_value(range_float_limited,PARAMETER,[{MINOR_A,MAJOR_A},{MINOR_A,MAJOR_B}])
	when
		is_float(MINOR_A), is_float(MAJOR_A),
		is_float(MINOR_A), is_float(MAJOR_B) ->

	a_yaws_params_primitives:float_range_limited(PARAMETER,{MINOR_A,MAJOR_A},{MINOR_A,MAJOR_B});

%% By Pattern
parameter_value(by_pattern,PARAMETER,[PATTERN,OUTPUT_TYPE]) ->

	a_yaws_params_primitives:by_pattern(PARAMETER,PATTERN,OUTPUT_TYPE).
