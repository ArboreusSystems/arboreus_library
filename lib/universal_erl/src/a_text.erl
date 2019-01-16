%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Text handler
%%%
%%% @end
%%% Created : 26. Март 2018 14:02
%%%-------------------------------------------------------------------
-module(a_text).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% API
-export([
	test/0,
	is_vowel/1,
	shortcut/2,
	clean_all_spaces/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Check the letter for being vowel
-spec is_vowel(Letter) -> true | false
	when
	Letter :: unicode:charlist() | pos_integer().

is_vowel("a") -> true; is_vowel("A") -> true; is_vowel(97) -> true; is_vowel(65) -> true;
is_vowel("e") -> true; is_vowel("E") -> true; is_vowel(101) -> true; is_vowel(69) -> true;
is_vowel("i") -> true; is_vowel("I") -> true; is_vowel(105) -> true; is_vowel(73) -> true;
is_vowel("o") -> true; is_vowel("O") -> true; is_vowel(111) -> true; is_vowel(79) -> true;
is_vowel("u") -> true; is_vowel("U") -> true; is_vowel(117) -> true; is_vowel(85) -> true;
is_vowel("y") -> true; is_vowel("Y") -> true; is_vowel(121) -> true; is_vowel(89) -> true;
is_vowel(_) -> false.


%% ----------------------------
%% @doc Wrapper for shortcut_string_handler/3
-spec shortcut(Name,Length) -> {ok,Output} | {error,Reason}
	when
	Name :: unicode:charlist() | unicode:unicode_binary(),
	Length :: pos_integer(),
	Output :: unicode:charlist(),
	Reason :: wrong_length.

shortcut(Name,Length) when is_list(Name) ->
	Name_length = length(clean_all_spaces(Name)),
	if
		Name_length < Length -> {error,wrong_length};
		true -> shortcut_string_handler(Name,Length,"")
	end;
shortcut(Name_bin,Length) ->
	shortcut(unicode:characters_to_list(Name_bin),Length).


%% ----------------------------
%% @doc Make non-vowel shortcut from text
-spec shortcut_string_handler(Text,Length,Output) -> {ok,Output} | {error,Reason}
	when
	Text :: unicode:charlist(),
	Length :: pos_integer(),
	Output :: unicode:charlist(),
	Reason :: wrong_length.

shortcut_string_handler(_,0,Output) ->
	{ok,Output};
shortcut_string_handler([],Length,_) when Length > 0 ->
	{error,wrong_length};
shortcut_string_handler([Letter|String],Length,Output) ->
	io:format("Letter: ~p~n",[Letter]),
	case is_vowel(Letter) of
		true -> shortcut_string_handler(String,Length,Output);
		_ -> shortcut_string_handler(String,Length-1,lists:append(Output,[Letter]))
	end.


%% ----------------------------
%% @doc Clearing all spaces from text
-spec clean_all_spaces(Text::unicode:charlist()) -> unicode:charlist().

clean_all_spaces(Text) ->
	re:replace(Text,"\\s+","",[global,{return,list}]).