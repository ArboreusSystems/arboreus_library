%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2016, http://arboreus.system
%%% @doc Yaws appmode json output generator
%%%
%%% @end
%%% Created : 13. Февр. 2016 14:36
%%%-------------------------------------------------------------------
-module(a_yaws_output_json).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% System include
-include("../include/types/types_general.hrl").
-include("../include/constants/a_constants_general.hrl").

%% API
-export([
	test/0,
	make/4
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Return a list prepared for Yaws appmod within
%% XML formated information from Mnesia DB
-spec make(Datum,Data_module,Output_type,Output_file_name) -> list()
	when
	Datum :: list() | tuple(),
	Data_module :: atom(),
	Output_type :: atom(),
	Output_file_name :: string().

make({atomic,[]},_,_,_) ->
	[
		a_yaws_http_headers:cache(no),
		?APPLICATION_HEADER_OK
	];
make({atomic,Db_responce},Data_module,Output_type,Output_file_name)
	when
		is_list(Db_responce),
		is_atom(Data_module),
		is_atom(Output_type),
		is_list(Output_file_name) ->
	[
		a_yaws_http_headers:json(Output_type,Output_file_name),
		?APPLICATION_HEADER_OK,
		{'ehtml',[
			unicode:characters_to_list(<<
				("{\"answer\":[")/utf8,
				(json(Data_module,{atomic,Db_responce},<<>>))/binary,
				("]}")/utf8
			>>)
		]}
	];
make([{count,Count},{atomic,Db_responce}],Data_module,Output_type,Output_file_name)
	when
		is_integer(Count),
		is_list(Db_responce),
		is_atom(Data_module),
		is_atom(Output_type),
		is_list(Output_file_name) ->
	[
		a_yaws_http_headers:xml(Output_type,Output_file_name),
		?APPLICATION_HEADER_OK,
		{'ehtml',[
			unicode:characters_to_list(<<
				("{\"answer\":[")/utf8,
				("{\"count\":")/utf8,(integer_to_binary(Count))/binary,("},")/utf8,
				(json(Data_module,{atomic,Db_responce},<<>>))/binary,
				("]}")/utf8
			>>)
		]}
	];
make(_,_,_,_) ->
	[
		a_yaws_http_headers:cache(no),
		?APPLICATION_HEADER_ERROR("wrong_output_json_make_parameters")
	].


%% ----------------------------
%% @doc Return unicode binary within JSON-object from record transformed to proplist
-spec json_from_proplist([{Key_in,Value_in}|List],Json_inner) -> unicode:chardata() | {error,_Reason}
	when
	Key_in :: atom(),
	Value_in :: atom() | integer() | unicode:chardata() | byte(),
	List :: list(),
	Json_inner :: byte().

json_from_proplist([],Json_inner) -> <<"{",Json_inner/binary,"}">>;
json_from_proplist([{Key_in,Value_in}|List],Json_inner) when is_atom(Key_in) ->
	Json_inner_out = fun() ->
		if
			List /= [] ->
				<<Json_inner/binary,("\"")/utf8,(a_var:to_binary(Key_in))/binary,
					("\":")/utf8,(a_yaws_output:value(Value_in))/binary,(",")/utf8>>;
			true ->
				<<Json_inner/binary,("\"")/utf8,(a_var:to_binary(Key_in))/binary,
					("\":")/utf8,(a_yaws_output:value(Value_in))/binary>>
		end
	end,
	json_from_proplist(List,Json_inner_out());
json_from_proplist([{_,_}|_],_) -> {error,wrong_list};
json_from_proplist(_,_) -> {error,badarg}.


%% ----------------------------
%% @doc Return unicode binary within JSON-object from proplist of records
-spec json(Data_module,Records,Json_binary) -> unicode:chardata() | {error,_Reason}
	when
	Data_module :: atom(),
	Records :: {_,Record_list::list()},
	Json_binary :: byte().

json(Data_module,{_,Row_list},Json_binary) -> json(Data_module,Row_list,Json_binary);
json(_,[],Json_binary) -> Json_binary;
json(Data_module,[Row|Row_list],Json_binary) ->
	Coma = fun() ->
		if
			Row_list == [] -> <<>> ;
			true -> <<(",")/utf8>>
		end
	end,
	json(Data_module,Row_list,<<
		Json_binary/binary,
		(json_from_proplist(a_proplists:from_record(Data_module,Row),<<>>))/binary,
		(Coma())/binary
	>>).