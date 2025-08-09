%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2025, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 06. Aug 2025 11:44
%%%-------------------------------------------------------------------
-module(a_yaws_params_primitives).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% System includes

%% Application includes
-include("a_includes.hrl").

%% API
-export([

	test/0,

	by_pattern/3,

	float/1,float_positive/1,float_negative/1,
	float_from_list/2,float_ranged/3,
	float_range/2,float_range_limited/3,

	integer/1,integer_positive/1,integer_negative/1,
	integer_from_list/2,integer_ranged/3,
	integer_range/2,integer_range_limited/3,

	utf_binary/1,utf_binary_limited/2,
	utf_binary_ranged/3,utf_binary_except/3,

	id/3,id_or_null/5,id_ranged/4,id_ranged_or_null/5,
	id_md5/2,id_md4/2,

	password/2,password_ranged/3,
	atom/1,atom_from_list/2,
	boolean/1,boolean_integer/1,
	latin_name/3,latin_name_ranged/4,
	base64/2,base64_encoded/2,
	ip_v4/2,ip_v4_range/2,
	ip_v6/2,
	fqdn/2,
	email/2,
	numerical/3,
	time/3

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Check parameter by Regex pattern
-spec by_pattern(PARAMETER,PATTERN,OUTPUT_TYPE) -> CHECKED_PARAMETER | nomatch
	when
		PARAMETER :: a_utf_text_string() | a_utf_text_binary(),
		PATTERN :: a_utf_text_string() | a_utf_text_binary(),
		OUTPUT_TYPE :: string | binary,
		CHECKED_PARAMETER :: a_utf_text_string() | a_utf_text_binary().

by_pattern(PARAMETER,PATTERN,OUTPUT_TYPE) when is_list(PARAMETER), is_list(PATTERN) ->

	by_pattern(list_to_binary(PARAMETER),list_to_binary(PATTERN),OUTPUT_TYPE);

by_pattern(PARAMETER,PATTERN,OUTPUT_TYPE) when is_binary(PARAMETER), is_binary(PATTERN) ->

	case re:run(PARAMETER,PATTERN) of
		nomatch ->
			nomatch;
		{match,_} ->
			case OUTPUT_TYPE of
				binary -> PARAMETER;
				string -> unicode:characters_to_list(PARAMETER)
			end
	end.


%% ----------------------------
%% @doc Check float parameter
-spec float(PARAMETER) -> FLOAT | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		FLOAT :: float().

float(PARAMETER) ->

	try list_to_float(PARAMETER)
	catch _:_ -> nomatch end.


%% ----------------------------
%% @doc Check float positive parameter
-spec float_positive(PARAMETER) -> FLOAT_POSITIVE | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		FLOAT_POSITIVE :: float().

float_positive(PARAMETER) ->

	case a_yaws_params_primitives:float(PARAMETER) of
		nomatch ->
			nomatch;
		FLOAT ->
			if
				FLOAT >= 0 -> FLOAT;
				true -> nomatch
			end
	end.


%% ----------------------------
%% @doc Check float negative parameter
-spec float_negative(PARAMETER) -> FLOAT_NEGATIVE | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		FLOAT_NEGATIVE :: float().

float_negative(PARAMETER) ->

	case a_yaws_params_primitives:float(PARAMETER) of
		nomatch ->
			nomatch;
		FLOAT ->
			if
				FLOAT < 0 -> FLOAT;
				true -> nomatch
			end
	end.


%% ----------------------------
%% @doc Check float from list parameter
-spec float_from_list(PARAMETER,LIST_OF_FLOATS) -> FLOAT_FROM_LIST | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		LIST_OF_FLOATS :: [float()],
		FLOAT_FROM_LIST :: float().

float_from_list(PARAMETER,LIST) ->

	case a_yaws_params_primitives:float(PARAMETER) of
		nomatch ->
			nomatch;
		FLOAT ->
			case lists:member(FLOAT,LIST) of
				true -> FLOAT;
				false -> nomatch
			end
	end.


%% ----------------------------
%% @doc Check ranged float parameter
-spec float_ranged(PARAMETER,MINOR,MAJOR) -> FLOAT_RANGED | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		MINOR :: float(),
		MAJOR :: float(),
		FLOAT_RANGED :: float().

float_ranged(PARAMETER,MINOR,MAJOR) when MINOR > MAJOR ->

	float_ranged(PARAMETER,MAJOR,MINOR);

float_ranged(PARAMETER,MINOR,MAJOR) ->

	case a_yaws_params_primitives:float(PARAMETER) of
		nomatch ->
			nomatch;
		FLOAT ->
			if
				FLOAT =< MAJOR ->
					if
						FLOAT >= MINOR -> FLOAT;
						true -> nomatch
					end;
				true ->
					nomatch
			end
	end.


%% ----------------------------
%% @doc Check float range parameter
-spec float_range(PARAMETER,TYPE) -> RANGE | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		TYPE :: range_pos_float | range_neg_float | range_float,
		RANGE :: {MINOR,MAJOR},
		MINOR :: integer(),
		MAJOR :: integer().

float_range(PARAMETER,TYPE) ->

	PATTERN = case TYPE of
		range_pos_float -> "^([0-9]*\.[0-9]*)\:([0-9]*\.[0-9]*)$";
		range_neg_float -> "^(\-[0-9]{1,}\.[0-9]{1,}|0\.0)\:(\-[0-9]{1,}\.[0-9]{1,}|0\.0)$";
		range_float -> "^(\-?[0-9]{1,}\.[0-9]{1,})\:(\-?[0-9]{1,}\.[0-9]{1,})$"
	end,

	case re:split(PARAMETER,PATTERN,[{return,list}]) of
		[_,VALUE_STRING_1,VALUE_STRING_2,_] ->
			VALUE_1 = list_to_float(VALUE_STRING_1),
			VALUE_2 = list_to_float(VALUE_STRING_2),
			if
				VALUE_1 > VALUE_2 ->
					MAJOR = VALUE_1, MINOR = VALUE_2;
				true ->
					MAJOR = VALUE_2, MINOR = VALUE_1
			end,
			{MINOR,MAJOR};
		[_] ->
			nomatch;
		_ ->
			nomatch
	end.


%% ----------------------------
%% @doc Check limited float range
-spec float_range_limited(PARAMETER,{MINOR_A,MAJOR_A},{MINOR_B,MAJOR_B}) -> RANGE | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		MINOR_A :: float(),
		MAJOR_A :: float(),
		MINOR_B :: float(),
		MAJOR_B :: float(),
		RANGE :: {MINOR,MAJOR},
		MINOR :: integer(),
		MAJOR :: integer().

float_range_limited(PARAMETER,{MINOR_A,MAJOR_A},{MINOR_B,MAJOR_B})
	when MINOR_A > MAJOR_A ->

	float_range_limited(PARAMETER,{MAJOR_A,MINOR_A},{MINOR_B,MAJOR_B});

float_range_limited(PARAMETER,{MINOR_A,MAJOR_A},{MINOR_B,MAJOR_B})
	when MINOR_B > MAJOR_B ->

	float_range_limited(PARAMETER,{MAJOR_A,MINOR_A},{MAJOR_B,MINOR_B});

float_range_limited(PARAMETER,{MINOR_A,MAJOR_A},{MINOR_B,MAJOR_B})
	when
		is_float(MINOR_A), is_float(MAJOR_A),
		is_float(MINOR_B), is_float(MAJOR_B) ->

	case float_range(PARAMETER,range_float) of
		{A,B} ->
			if
				A =< MAJOR_A; A >= MINOR_A ->
					if
						B =< MAJOR_B; B >= MINOR_B -> {A,B};
						true -> nomatch
					end;
				true ->
					nomatch
			end;
		nomatch ->
			nomatch
	end.


%% ----------------------------
%% @doc Check integer parameter
-spec integer(PARAMETER) -> INTEGER | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		INTEGER :: integer().

integer(PARAMETER) ->

	try list_to_integer(PARAMETER)
	catch _:_ -> nomatch end.


%% ----------------------------
%% @doc Check positive integer parameter
-spec integer_positive(PARAMETER) -> INTEGER_POSITIVE | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		INTEGER_POSITIVE :: pos_integer().

integer_positive(PARAMETER) ->

	case a_yaws_params_primitives:integer(PARAMETER) of
		nomatch ->
			nomatch;
		INTEGER ->
			if
				INTEGER >= 0 -> INTEGER;
				true -> nomatch
			end
	end.


%% ----------------------------
%% @doc Check negative integer parameter
-spec integer_negative(PARAMETER) -> INTEGER_NEGATIVE | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		INTEGER_NEGATIVE :: neg_integer().

integer_negative(PARAMETER) ->

	case a_yaws_params_primitives:integer(PARAMETER) of
		nomatch ->
			nomatch;
		INTEGER ->
			if
				INTEGER < 0 -> INTEGER;
				true -> nomatch
			end
	end.


%% ----------------------------
%% @doc Check float from list parameter
-spec integer_from_list(PARAMETER,LIST_OF_INTEGERS) -> INTEGER_FROM_LIST | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		LIST_OF_INTEGERS :: [integer()],
		INTEGER_FROM_LIST :: integer().

integer_from_list(PARAMETER,LIST) ->

	case a_yaws_params_primitives:integer(PARAMETER) of
		nomatch ->
			nomatch;
		FLOAT ->
			case lists:member(FLOAT,LIST) of
				true -> FLOAT;
				false -> nomatch
			end
	end.


%% ----------------------------
%% @doc Check ranged integer parameter
-spec integer_ranged(PARAMETER,MINOR,MAJOR) -> INTEGER_RANGED | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		MINOR :: integer(),
		MAJOR :: integer(),
		INTEGER_RANGED :: integer().

integer_ranged(PARAMETER,MINOR,MAJOR) when MINOR > MAJOR ->

	integer_ranged(PARAMETER,MAJOR,MINOR);

integer_ranged(PARAMETER,MINOR,MAJOR) ->

	case a_yaws_params_primitives:integer(PARAMETER) of
		nomatch ->
			nomatch;
		FLOAT ->
			if
				FLOAT =< MAJOR ->
					if
						FLOAT >= MINOR -> FLOAT;
						true -> nomatch
					end;
				true ->
					nomatch
			end
	end.


%% ----------------------------
%% @doc Check float range parameter
-spec integer_range(PARAMETER,TYPE) -> RANGE | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		TYPE :: range_pos_integer | range_neg_integer | range_integer,
		RANGE :: {MINOR,MAJOR},
		MINOR :: integer(),
		MAJOR :: integer().

integer_range(PARAMETER,TYPE) ->

	PATTERN = case TYPE of
		range_pos_integer -> "^([0-9]{1,})\:([0-9]{1,})$";
		range_neg_integer -> "^(\-[0-9]{1,}|0)\:(\-[0-9]{1,}|0)$";
		range_integer -> "^(\-?[0-9]{1,})\:(\-?[0-9]{1,})$"
	end,

	case re:split(PARAMETER,PATTERN,[{return,list}]) of
		[_,VALUE_STRING_1,VALUE_STRING_2,_] ->
			VALUE_1 = list_to_integer(VALUE_STRING_1),
			VALUE_2 = list_to_integer(VALUE_STRING_2),
			if
				VALUE_1 > VALUE_2 ->
					MAJOR = VALUE_1, MINOR = VALUE_2;
				true ->
					MAJOR = VALUE_2, MINOR = VALUE_1
			end,
			{MINOR,MAJOR};
		[_] ->
			nomatch;
		_ ->
			nomatch
	end.


%% ----------------------------
%% @doc Check limited integer range
-spec integer_range_limited(PARAMETER,{MINOR_A,MAJOR_A},{MINOR_B,MAJOR_B}) -> RANGE | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		MINOR_A :: integer(),
		MAJOR_A :: integer(),
		MINOR_B :: integer(),
		MAJOR_B :: integer(),
		RANGE :: {MINOR,MAJOR},
		MINOR :: integer(),
		MAJOR :: integer().

integer_range_limited(PARAMETER,{MINOR_A,MAJOR_A},{MINOR_B,MAJOR_B})
	when MINOR_A > MAJOR_A ->

	integer_range_limited(PARAMETER,{MAJOR_A,MINOR_A},{MINOR_B,MAJOR_B});

integer_range_limited(PARAMETER,{MINOR_A,MAJOR_A},{MINOR_B,MAJOR_B})
	when MINOR_B > MAJOR_B ->

	integer_range_limited(PARAMETER,{MAJOR_A,MINOR_A},{MAJOR_B,MINOR_B});

integer_range_limited(PARAMETER,{MINOR_A,MAJOR_A},{MINOR_B,MAJOR_B})
	when
		is_integer(MINOR_A), is_integer(MAJOR_A),
		is_integer(MINOR_B), is_integer(MAJOR_B) ->

	case integer_range(PARAMETER,float) of
		{A,B} ->
			if
				A =< MAJOR_A; A >= MINOR_A ->
					if
						B =< MAJOR_B; B >= MINOR_B -> {A,B};
						true -> nomatch
					end;
				true ->
					nomatch
			end;
		nomatch ->
			nomatch
	end.


%% ----------------------------
%% @doc Check UTF binary parameter
-spec utf_binary(PARAMETER) -> UTF_BINARY | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		UTF_BINARY :: a_utf_text_binary().

utf_binary(PARAMETER) when is_list(PARAMETER) ->

	unicode:characters_to_binary(PARAMETER).


%% ----------------------------
%% @doc Check UTF binary parameter limited by length
-spec utf_binary_limited(PARAMETER,LIMIT) -> UTF_BINARY | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		LIMIT :: {more_equal,LENGTH} | {less_equal,LENGTH} | {size,LENGTH},
		LENGTH :: pos_integer(),
		UTF_BINARY :: a_utf_text_binary().

utf_binary_limited(PARAMETER,{more_equal,LENGTH})
	when is_integer(LENGTH), LENGTH >= 1 ->

	UTF_BINARY = unicode:characters_to_binary(PARAMETER),
	if
		byte_size(UTF_BINARY) >= LENGTH -> UTF_BINARY;
		true -> nomatch
	end;

utf_binary_limited(PARAMETER,{less_equal,LENGTH})
	when is_integer(LENGTH), LENGTH >= 1 ->

	UTF_BINARY = unicode:characters_to_binary(PARAMETER),
	if
		byte_size(UTF_BINARY) =< LENGTH -> UTF_BINARY;
		true -> nomatch
	end;

utf_binary_limited(PARAMETER,{size,LENGTH})
	when is_integer(LENGTH), LENGTH >= 1 ->

	UTF_BINARY = unicode:characters_to_binary(PARAMETER),
	if
		byte_size(UTF_BINARY) == LENGTH -> UTF_BINARY;
		true -> nomatch
	end.


%% ----------------------------
%% @doc Check ranged UTF binary parameter
-spec utf_binary_ranged(PARAMETER,MINOR,MAJOR) -> UTF_BINARY | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		MINOR :: pos_integer(),
		MAJOR :: pos_integer(),
		UTF_BINARY :: a_utf_text_binary().

utf_binary_ranged(PARAMETER,MINOR,MAJOR) when MINOR > MAJOR ->

	utf_binary_ranged(PARAMETER,MAJOR,MINOR);

utf_binary_ranged(PARAMETER,MINOR,MAJOR) when MINOR >= 1 ->

	UTF_BINARY = unicode:characters_to_binary(PARAMETER),
	SIZE = byte_size(UTF_BINARY),
	if
		SIZE >= MINOR, SIZE =< MAJOR -> UTF_BINARY;
		true -> nomatch
	end.


%% ----------------------------
%% @doc Check UTF binary parameter with exception chars
-spec utf_binary_except(PARAMETER,EXCEPTION_CHARS,LENGTH_TYPE) -> UTF_BINARY | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		EXCEPTION_CHARS :: a_utf_text_string(),
		LENGTH_TYPE :: free | {more_equal,LENGTH} | {less_equal,LENGTH} | {size,LENGTH} | {range,MINOR,MAJOR},
		LENGTH :: pos_integer(),
		MINOR :: pos_integer(),
		MAJOR :: pos_integer(),
		UTF_BINARY :: a_utf_text_binary().

utf_binary_except(PARAMETER,EXCEPTION_CHARS,LENGTH_TYPE) ->

	case io_lib:char_list(EXCEPTION_CHARS) of
		true ->

			UTF_BINARY = unicode:characters_to_binary(PARAMETER),
			EXCEPTION = unicode:characters_to_binary(EXCEPTION_CHARS),

			PATTERN = fun() ->
				case LENGTH_TYPE of
					free ->
						<<("^((?![")/utf8,(EXCEPTION)/binary,("]).){1,}$")/utf8>>;
					{less_equal,LENGTH} ->
						if
							is_integer(LENGTH), LENGTH >= 2 ->
								<<("^((?![")/utf8,(EXCEPTION)/binary,
									("]).){1,")/utf8,(integer_to_binary(LENGTH))/binary,
									("}$")/utf8>>;
							true ->
								nomatch
						end;
					{size,LENGTH} ->
						if
							is_integer(LENGTH), LENGTH >= 2 ->
								<<("^((?![")/utf8,(EXCEPTION)/binary,
									("]).){")/utf8,(integer_to_binary(LENGTH))/binary,
									("}$")/utf8>>;
							true ->
								nomatch
						end;
					{range,MINOR,MAJOR} ->
						if
							is_integer(MINOR),is_integer(MAJOR), MINOR >= 1, MAJOR > MINOR ->
								<<("^((?![")/utf8,(EXCEPTION)/binary,
									("]).){")/utf8,(integer_to_binary(MINOR))/binary,
									(",")/utf8,(integer_to_binary(MAJOR))/binary,
									("}$")/utf8>>;
							true ->
								nomatch
						end
				end
			end,

			case PATTERN() of
				{error,REASON} ->
					{error,REASON};
				PATTERN_BINARY ->
					case re:run(UTF_BINARY,PATTERN_BINARY) of
						nomatch -> nomatch;
						{match,_} -> UTF_BINARY
					end
			end;

		false ->
			nomatch
	end.


%% ----------------------------
%% @doc Check ID of defined length
-spec id(PARAMETER,LENGTH,OUTPUT_TYPE) -> ID | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		LENGTH :: pos_integer(),
		OUTPUT_TYPE :: binary | string,
		ID :: a_utf_text_binary() | a_utf_text_string().

id(PARAMETER,LENGTH,OUTPUT_TYPE) ->

	if
		LENGTH > 0 ->
			PATTERN = <<
				("^[a-zA-Z0-9]{")/utf8,
				(integer_to_binary(LENGTH))/binary,
				("}$")/utf8
			>>,
			PARAMETER_BINARY = unicode:characters_to_binary(PARAMETER),
			case re:run(PARAMETER_BINARY,PATTERN) of
				nomatch ->
					nomatch;
				{match,_} ->
					case OUTPUT_TYPE of
						binary -> PARAMETER_BINARY;
						string -> PARAMETER
					end
			end;
		true ->
			nomatch
	end.


%% ----------------------------
%% @doc Check ID of defined length or null value
-spec id_or_null(PARAMETER,LENGTH,NULL,OUTPUT_NULL,OUTPUT_TYPE) -> ID | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		LENGTH :: pos_integer(),
		NULL :: a_utf_text_string(),
		OUTPUT_NULL :: any(),
		OUTPUT_TYPE :: binary | string,
		ID :: a_utf_text_binary() | a_utf_text_string() | null.

id_or_null(PARAMETER,_LENGTH,NULL,OUTPUT_NULL,_OUTPUT_TYPE)
	when PARAMETER =:= NULL ->

	OUTPUT_NULL;

id_or_null(PARAMETER,LENGTH,_NULL,_OUTPUT_NULL,OUTPUT_TYPE) ->

	id(PARAMETER,LENGTH,OUTPUT_TYPE).


%% ----------------------------
%% @doc Check ranged ID
-spec id_ranged(PARAMETER,MINOR,MAJOR,OUTPUT_TYPE) -> ID | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		MINOR :: pos_integer(),
		MAJOR :: pos_integer(),
		OUTPUT_TYPE :: binary | string,
		ID :: a_utf_text_binary() | a_utf_text_string().

id_ranged(PARAMETER,MINOR,MAJOR,OUTPUT_TYPE) when MINOR > MAJOR ->

	id_ranged(PARAMETER,MAJOR,MINOR,OUTPUT_TYPE);

id_ranged(PARAMETER,MINOR,MAJOR,OUTPUT_TYPE) ->

	LENGTH =  length(PARAMETER),
	if
		LENGTH > MAJOR -> nomatch;
		LENGTH < MINOR -> nomatch;
		true -> id(PARAMETER,LENGTH,OUTPUT_TYPE)
	end.


%% ----------------------------
%% @doc Check ranged ID or null value
-spec id_ranged_or_null(PARAMETER,MINOR,MAJOR,NULL,OUTPUT_TYPE) -> ID | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		MINOR :: pos_integer(),
		MAJOR :: pos_integer(),
		NULL :: a_utf_text_string(),
		OUTPUT_TYPE :: binary | string,
		ID :: a_utf_text_binary() | a_utf_text_string() | null.

id_ranged_or_null(PARAMETER,_MINOR,_MAJOR,NULL,_OUTPUT_TYPE) when PARAMETER =:= NULL ->

	null;

id_ranged_or_null(PARAMETER,MINOR,MAJOR,_NULL,OUTPUT_TYPE) ->

	id_ranged(PARAMETER,MINOR,MAJOR,OUTPUT_TYPE).


%% ----------------------------
%% @doc Check md5 hashed ID
-spec id_md5(PARAMETER,OUTPUT_TYPE) -> ID_MD5 | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		OUTPUT_TYPE :: binary | string,
		ID_MD5 :: a_utf_text_string() | a_utf_text_binary().

id_md5(PARAMETER,OUTPUT_TYPE) -> id(PARAMETER,32,OUTPUT_TYPE).


%% ----------------------------
%% @doc Check md4 hashed ID
-spec id_md4(PARAMETER,OUTPUT_TYPE) -> ID_MD4 | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		OUTPUT_TYPE :: binary | string,
		ID_MD4 :: a_utf_text_string() | a_utf_text_binary().

id_md4(PARAMETER,OUTPUT_TYPE) -> id(PARAMETER,32,OUTPUT_TYPE).


%% ----------------------------
%% @doc Check password parameter
-spec password(PARAMETER,MINIMUM_LENGTH) -> PASSWORD | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		MINIMUM_LENGTH :: pos_integer(),
		PASSWORD :: a_utf_text_binary().

password(PARAMETER,MINIMUM_LENGTH) ->

	utf_binary_limited(PARAMETER,{more_equal,MINIMUM_LENGTH}).


%% ----------------------------
%% @doc Check ranged password parameter
-spec password_ranged(PARAMETER,MINOR,MAJOR) -> PASSWORD | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		MINOR :: pos_integer(),
		MAJOR :: pos_integer(),
		PASSWORD :: a_utf_text_binary().

password_ranged(PARAMETER,MINOR,MAJOR) when MINOR > MAJOR ->

	password_ranged(PARAMETER,MAJOR,MINOR);

password_ranged(PARAMETER,MINOR,MAJOR) ->

	utf_binary_ranged(PARAMETER,MINOR,MAJOR).


%% ----------------------------
%% @doc Check atom parameter
-spec atom(PARAMETER) -> ATOM | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		ATOM :: atom().

atom(PARAMETER) ->

	try list_to_existing_atom(PARAMETER)
	catch _:_ -> nomatch end.


%% ----------------------------
%% @doc Check atom from list parameter
-spec atom_from_list(PARAMETER,LIST) -> ATOM_FROM_LIST | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		LIST :: [atom()],
		ATOM_FROM_LIST :: atom().

atom_from_list(PARAMETER,LIST) ->

	case a_yaws_params_primitives:atom(PARAMETER) of
		nomatch ->
			nomatch;
		ATOM ->
			case lists:member(ATOM,LIST) of
				true -> ATOM;
				false -> nomatch
			end
	end.


%% ----------------------------
%% @doc Check boolean parameter
-spec boolean(PARAMETER) -> BOOLEAN | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		BOOLEAN :: boolean().

boolean(PARAMETER) ->

	case PARAMETER of
		"true" -> true;
		"1" -> true;
		"false" -> false;
		"0" -> false;
		PARAMETER -> nomatch
	end.


%% ----------------------------
%% @doc Check boolean integer parameter
-spec boolean_integer(PARAMETER) -> BOOLEAN_INTEGER | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		BOOLEAN_INTEGER :: 1 | 0.

boolean_integer(PARAMETER) ->

	case PARAMETER of
		"true" -> 1;
		"1" -> 1;
		"false" -> 0;
		"0" -> 0;
		PARAMETER -> nomatch
	end.


%% ----------------------------
%% @doc Check latin name parameter
-spec latin_name(PARAMETER,LENGTH,OUTPUT_TYPE) -> LATIN_NAME | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		LENGTH :: pos_integer(),
		LATIN_NAME :: a_utf_text_string() | a_utf_text_binary(),
		OUTPUT_TYPE :: binary | string.

latin_name(PARAMETER,LENGTH,OUTPUT_TYPE) ->

	PATTERN = fun() ->
		case LENGTH of
			free ->
				<<("^[a-zA-Z]{1}[a-zA-Z0-9\_\-]{1,}$")/utf8>>;
			_ ->
				<<("^[a-zA-Z]{1}[a-zA-Z0-9\_\-]{1,")/utf8,
					(integer_to_binary(LENGTH - 1))/binary,
					("}$")/utf8>>
		end
	end,

	BINARY_PARAMETER = unicode:characters_to_binary(PARAMETER),
	case re:run(BINARY_PARAMETER,PATTERN()) of
		nomatch ->
			nomatch;
		{match,_} ->
			case OUTPUT_TYPE of
				binary -> BINARY_PARAMETER;
				string -> PARAMETER
			end
	end.


%% ----------------------------
%% @doc Check latin name ranged parameter
-spec latin_name_ranged(PARAMETER,MINOR,MAJOR,OUTPUT_TYPE) -> LATIN_NAME | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		MINOR :: pos_integer(),
		MAJOR :: pos_integer(),
		OUTPUT_TYPE :: binary | string,
		LATIN_NAME :: a_utf_text_string() | a_utf_text_binary().

latin_name_ranged(PARAMETER,MINOR,MAJOR,OUTPUT_TYPE) when MINOR > MAJOR ->

	latin_name_ranged(PARAMETER,MAJOR,MINOR,OUTPUT_TYPE);

latin_name_ranged(PARAMETER,MINOR,MAJOR,OUTPUT_TYPE) ->

	PATTERN = <<
		("^[a-zA-Z]{1}[a-zA-Z0-9\_\-]{")/utf8,
		(integer_to_binary(MINOR - 1))/binary,
		(",")/utf8,
		(integer_to_binary(MAJOR - 1))/binary,
		("}$")/utf8
	>>,

	BINARY_PARAMETER = unicode:characters_to_binary(PARAMETER),
	case re:run(BINARY_PARAMETER,PATTERN) of
		nomatch ->
			nomatch;
		{match,_} ->
			case OUTPUT_TYPE of
				binary -> BINARY_PARAMETER;
				string -> PARAMETER
			end
	end.


%% ----------------------------
%% @doc Check base64 decoded parameter
-spec base64(PARAMETER,OUTPUT_TYPE) -> BASE64 | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		OUTPUT_TYPE :: binary | string,
		BASE64 :: a_utf_text_string() | a_utf_text_binary().

base64(PARAMETER,OUTPUT_TYPE) ->

	PATTERN = "^([a-zA-Z0-9\=\+\/]{0,})$",
	try
		case re:run(PARAMETER,PATTERN) of
			nomatch -> nomatch;
			{match,_} ->
				case OUTPUT_TYPE of
					binary -> list_to_binary(PARAMETER);
					string -> PARAMETER
				end
		end
	catch _:_ ->
		nomatch
	end.


%% ----------------------------
%% @doc Check base64 decoded parameter with encoding
-spec base64_encoded(PARAMETER,OUTPUT_TYPE) -> BASE64 | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		OUTPUT_TYPE :: binary | string,
		BASE64 :: a_utf_text_string() | a_utf_text_binary().

base64_encoded(PARAMETER,OUTPUT_TYPE) ->

	PATTERN = "^([a-zA-Z0-9\=\+\/]{0,})$",
	try
		case re:run(PARAMETER,PATTERN) of
			nomatch ->
				nomatch;
			{match,_} ->

				ENCODED_BASE64 =
					try binary_to_list(base64:decode(PARAMETER))
				    catch _:_ -> nomatch
				    end,

				case ENCODED_BASE64 of
					nomatch ->
						nomatch;
					ENCODED_BASE64 ->
						case OUTPUT_TYPE of
							binary -> list_to_binary(ENCODED_BASE64);
							string -> ENCODED_BASE64
						end
				end
		end
	catch _:_ ->
		nomatch
	end.


%% ----------------------------
%% @doc Check IPv4 parameter
-spec ip_v4(PARAMETER,OUTPUT_TYPE) -> IP_V4 | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		OUTPUT_TYPE :: string | binary | list | tuple | integer,
		IP_V4 :: a_utf_text_string() | a_utf_text_binary() | a_ipv4_list() | a_ipv4_tuple() | a_ipv4_integer().

ip_v4(PARAMETER,OUTPUT_TYPE) ->

	try
		{ok,IP_TUPLE} = inet:parse_ipv4_address(PARAMETER),
		case OUTPUT_TYPE of
			string -> PARAMETER;
			binary -> unicode:characters_to_binary(PARAMETER);
			list -> tuple_to_list(IP_TUPLE);
			tuple -> IP_TUPLE;
			integer -> a_net:ipv4_to_integer(IP_TUPLE)
		end
	catch _:_ ->
		nomatch
	end.


%% ----------------------------
%% @doc Check IP v4 range parameter
-spec ip_v4_range(PARAMETER,OUTPUT_TYPE) -> IP_V4_RANGE | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		OUTPUT_TYPE :: binary | string | tuple | list,
		IP_V4_RANGE :: a_utf_text_string() | a_utf_text_binary() | a_ipv4_list() | a_ipv4_tuple().

ip_v4_range(PARAMETER,OUTPUT_TYPE) ->

	PATTERN = "^([0-9]{1,3})\.([0-9]{1,3})\.([0-9]{1,3})\.([0-9]{1,3})\/([0-9]{1,3})$",
	case re:split(PARAMETER,PATTERN) of
		[_,A,B,C,D,E,_] ->
			INGETER_A = binary_to_integer(A),
			INTEGER_B = binary_to_integer(B),
			INGETER_C = binary_to_integer(C),
			INGETER_D = binary_to_integer(D),
			INGETER_E = binary_to_integer(E),
			if
				INGETER_A >= 0, INGETER_A =< 255,
				INTEGER_B >= 0, INTEGER_B =< 255,
				INGETER_C >= 0, INGETER_C =< 255,
				INGETER_D >= 0, INGETER_D =< 255,
				INGETER_E >= 0, INGETER_E =< 255 ->
					case OUTPUT_TYPE of
						binary -> unicode:characters_to_binary(PARAMETER);
						string -> PARAMETER;
						tuple -> {INGETER_A,INTEGER_B,INGETER_C,INGETER_D,INGETER_E};
						list -> [INGETER_A,INTEGER_B,INGETER_C,INGETER_D,INGETER_E]
					end;
				true ->
					nomatch
			end;
		[_] -> nomatch
	end.


%% ----------------------------
%% @doc Check IP v6 parameter
-spec ip_v6(PARAMETER,OUTPUT_TYPE) -> IP_V6 | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		OUTPUT_TYPE :: string | binary | list | tuple | integer,
		IP_V6 :: a_utf_text_string() | a_utf_text_binary() | a_ipv4_list() | a_ipv4_tuple() | a_ipv4_integer().

ip_v6(PARAMETER,OUTPUT_TYPE) ->

	try
		{ok,IP_TUPLE} = inet:parse_ipv6_address(PARAMETER),
		case OUTPUT_TYPE of
			string -> PARAMETER;
			binary -> unicode:characters_to_binary(PARAMETER);
			list -> tuple_to_list(IP_TUPLE);
			tuple -> IP_TUPLE;
			_ -> a_net:ipv6_to_integer(PARAMETER)
		end
	catch _:_ ->
		nomatch
	end.


%% ----------------------------
%% @doc Check FQDN parameter
-spec fqdn(PARAMETER,OUTPUT_TYPE) -> FQDN | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		OUTPUT_TYPE :: string | binary,
		FQDN :: a_utf_text_string() | a_utf_text_binary().

fqdn(PARAMETER,OUTPUT_TYPE) ->

	Pattern = "^(\.?([a-zA-Z0-9\-\_]{1,})){0,}$",
	case re:run(PARAMETER,Pattern) of
		nomatch ->
			nomatch;
		{match,_} ->
			case OUTPUT_TYPE of
				string -> PARAMETER;
				binary -> unicode:characters_to_binary(PARAMETER)
			end
	end.


%% ----------------------------
%% @doc Check email parameter
-spec email(PARAMETER,OUTPUT_TYPE) -> EMAIL | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		OUTPUT_TYPE :: string | binary,
		EMAIL :: a_utf_text_string() | a_utf_text_binary().

email(PARAMETER,OUTPUT_TYPE) ->

	PATTERN = "^([a-zA-Z0-9\.\_\-]{1,})\@([a-zA-Z0-9\.\_\-]{1,})$",
	case re:run(PARAMETER,PATTERN) of
		nomatch -> nomatch;
		{match,_} ->
			case OUTPUT_TYPE of
				string -> PARAMETER;
				binary -> unicode:characters_to_binary(PARAMETER)
			end
	end.


%% ----------------------------
%% @doc Check numerical string parameter
-spec numerical(PARAMETER,LENGTH_RULE,OUTPUT_TYPE) -> NUMERICAL | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		LENGTH_RULE :: {less_or_equal,LENGTH} | {equal,LENGTH} | {ranged,MINOR,MAJOR} | {more_then,LENGTH},
		LENGTH :: pos_integer(),
		MINOR :: pos_integer(),
		MAJOR :: pos_integer(),
		OUTPUT_TYPE :: string | binary,
		NUMERICAL :: a_utf_text_string() | a_utf_text_binary().

numerical(PARAMETER,LENGTH_RULE,OUTPUT_TYPE) ->

	LENGTH = case LENGTH_RULE of
		{less_or_equal,LENGTH_NUMBER} ->
			"{0," ++ integer_to_list(LENGTH_NUMBER) ++ "}";
		{equal,LENGTH_NUMBER} ->
			"{" ++ integer_to_list(LENGTH_NUMBER) ++ "}";
		{ranged,MINOR,MAJOR} ->
			if
				MINOR > MAJOR ->
					"{" ++ integer_to_list(MAJOR) ++ "," ++ integer_to_list(MINOR) ++ "}";
				true ->
					"{" ++ integer_to_list(MINOR) ++ "," ++ integer_to_list(MAJOR) ++ "}"
			end;
		{more_then,LENGTH_NUMBER} ->
			"{" ++ integer_to_list(LENGTH_NUMBER) ++ ",}"
	end,

	PATTERN = "^([0-9]" ++ LENGTH ++ ")$",
	case re:run(PARAMETER,PATTERN) of
		nomatch ->
			nomatch;
		{match,_} ->
			case OUTPUT_TYPE of
				string -> PARAMETER;
				binary -> unicode:characters_to_binary(PARAMETER)
			end
	end.


%% ----------------------------
%% @doc Check time parameter
-spec time(PARAMETER,FORMAT,OUTPUT_TYPE) -> TIME | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		FORMAT :: date_tuple | rfc822 | rfc850 | ansi,
		OUTPUT_TYPE :: string | binary | tuple,
		TIME :: a_utf_text_string() | a_utf_text_binary() | tuple().

time(PARAMETER,FORMAT,OUTPUT_TYPE) ->

	case OUTPUT_TYPE of
		string ->
			case a_time:from_formated(FORMAT,PARAMETER,tuple) of
				false -> nomatch;
				{error,_} -> nomatch;
				_ -> PARAMETER
			end;
		binary ->
			case a_time:from_formated(FORMAT,PARAMETER,tuple) of
				false -> nomatch;
				{error,_} -> nomatch;
				_ -> unicode:characters_to_binary(PARAMETER)
			end;
		_ ->
			case a_time:from_formated(FORMAT,PARAMETER,OUTPUT_TYPE) of
				false -> nomatch;
				{error,_} -> nomatch;
				Time -> Time
			end
	end.