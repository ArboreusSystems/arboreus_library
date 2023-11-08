%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc The variables handler
%%%
%%% @end
%%% Created : 10. Апр. 2018 14:40
%%%-------------------------------------------------------------------
-module(a_var).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% System include
-include_lib("../include/types/types_general.hrl").

%% API
-export([
	test/0,
	dump/2,
	to_string/1,
	to_binary/1,
	to_integer/1,
	inspector/1,inspector/2
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc The data inspector by type of data, return description and inspector
-spec inspector(Variable) -> {atom,function()}
	when
	Variable :: any().

inspector(Variable) when is_function(Variable) -> {function,(fun is_function/1)};
inspector(Variable) when is_list(Variable) -> {list,(fun is_list/1)};
inspector(Variable) when is_map(Variable) -> {map,(fun is_map/1)};
inspector(Variable) when is_pid(Variable) -> {pid,(fun is_pid/1)};
inspector(Variable) when is_port(Variable) -> {port,(fun is_port/1)};
inspector(Variable) when is_reference(Variable) -> {reference,(fun is_reference/1)};
inspector(Variable) when is_bitstring(Variable) -> {bitstring,(fun is_bitstring/1)};
inspector(Variable) when is_binary(Variable) -> {binary,(fun is_binary/1)};
inspector(Variable) when is_number(Variable) -> {number,(fun is_number/1)};
inspector(Variable) when is_tuple(Variable) -> {tuple,(fun is_tuple/1)};
inspector(Variable) when is_boolean(Variable) -> {boolean,(fun is_boolean/1)};
inspector(Variable) when is_atom(Variable) -> {atom,(fun is_atom/1)}.


%% ----------------------------
%% @doc The data inspector by type of data, return description or inspector
-spec inspector(Kind,Variable) -> Inspector | Description
	when
	Kind :: verificator | description,
	Variable :: any(),
	Inspector :: function(),
	Description :: atom().

inspector(verificator,Variable) ->
	{_,Inspector} = inspector(Variable),
	Inspector;
inspector(description,Variable) ->
	{Description,_} = inspector(Variable),
	Description.


%%-----------------------------------
%% @doc Write to file the variable value
-spec dump(Path,Variable) -> ok | {error, _Reason}
	when
	Path :: unix_path_string(),
	Variable :: any().

dump(Path,Variable) ->
	file:write_file(Path,io_lib:fwrite("~p.\n",[Variable])).


%%-----------------------------------
%% @doc Return string converted from binary
-spec to_string(Variable) -> utf_text()
	when
	Variable :: any().

to_string(String) when is_list(String) -> String;
to_string(Binary) when is_binary(Binary) -> unicode:characters_to_list(Binary);
to_string(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_string(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_string(Float) when is_float(Float) -> float_to_list(Float).


%%-----------------------------------
%% @doc Return binary within converted value, purposed for integer() or string() datatypes
-spec to_binary(Variable) -> byte()
	when
	Variable :: any().

to_binary(String) when is_list(String) -> unicode:characters_to_binary(String);
to_binary(Binary) when is_binary(Binary) -> Binary;
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom,utf8);
to_binary(Integer) when is_integer(Integer) -> integer_to_binary(Integer);
to_binary(Float) when is_float(Float) -> float_to_binary(Float).


%%-----------------------------------
%% @doc Return integer
-spec to_integer(Variable) -> integer()
	when
	Variable :: any().

to_integer(String) when is_list(String) -> list_to_integer(String);
to_integer(Integer) when is_integer(Integer) -> Integer;
to_integer(Float) when is_float(Float) -> list_to_integer(float_to_list(Float,[{decimals,0}]));
to_integer(Binary) when is_binary(Binary) -> binary_to_integer(Binary).