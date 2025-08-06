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

	atom/1,atom_from_list/2,

	boolean/1,boolean_integer/1

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