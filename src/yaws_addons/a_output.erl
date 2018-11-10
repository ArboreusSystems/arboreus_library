%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2016, http://arboreus.system
%%% @doc The output handler
%%%
%%% @end
%%% Created : 12. Февр. 2016 15:00
%%%-------------------------------------------------------------------
-module(a_output).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% API
-export([
	value/1
]).


%% ----------------------------
%% @doc Return unicode binary within converted value
-spec value(Value_in::any()) -> byte().

value({wrapped,Value_in}) when is_binary(Value_in) ->
	<<("\"")/utf8,Value_in/binary,("\"")/utf8>>;
value(Value_in) when Value_in == true; Value_in == false ->
	a_var:to_binary(Value_in);
value(Value_in) when is_integer(Value_in) ->
	a_var:to_binary(Value_in);
value(Value_in) when is_binary(Value_in) ->
	value({wrapped,Value_in});
value(Value_in) when is_atom(Value_in) ->
	value({wrapped,a_var:to_binary(Value_in)});
value(Value_in) when is_list(Value_in) ->
	case io_lib:char_list(Value_in) of
		true ->
			Value = unicode:characters_to_binary(Value_in),
			<<("\"")/utf8,Value/binary,("\"")/utf8>>;
		false -> value({wrapped,<<"{error,wrong_value_type}">>})
	end;
value(_) ->
	value({wrapped,<<"{error,wrong_value_type}">>}).