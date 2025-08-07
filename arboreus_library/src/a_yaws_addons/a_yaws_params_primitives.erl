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

	float/1,float_positive/1,float_negative/1,
	float_from_list/2,float_ranged/3,

	integer/1,integer_positive/1,integer_negative/1,
	integer_from_list/2,integer_ranged/3,

	id/3,id_or_null/4,id_ranged/4,id_ranged_or_null/5,
	id_md5/2,id_md4/2,

	atom/1,atom_from_list/2,
	boolean/1,boolean_integer/1,
	latin_name/3,latin_name_ranged/4,
	base64/2, base64_encoded/2

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


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
-spec id_or_null(PARAMETER,LENGTH,NULL,OUTPUT_TYPE) -> ID | nomatch
	when
		PARAMETER :: a_utf_text_string(),
		LENGTH :: pos_integer(),
		NULL :: a_utf_text_string(),
		OUTPUT_TYPE :: binary | string,
		ID :: a_utf_text_binary() | a_utf_text_string() | null.

id_or_null(PARAMETER,_LENGTH,NULL,_OUTPUT_TYPE) when PARAMETER =:= NULL -> null;

id_or_null(PARAMETER,LENGTH,_NULL,OUTPUT_TYPE) -> id(PARAMETER,LENGTH,OUTPUT_TYPE).


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

