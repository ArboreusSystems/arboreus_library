%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Configurations files parsing module
%%%
%%% @end
%%% Created : 11/05/2018 at 12:59
%%%-------------------------------------------------------------------
-module(a_config).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types
-include_lib("../include/types/types_a_general.hrl").

%% Data models

%% API
-export([
	test/0,
	parse_file/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_config) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Config = [{string,"test"},
		{list_of_integers,[1,2,3,4,5,6]},
		{list_of_atom,[one,two,three]},
		{integer,123456789}],
	Config_source = <<
		("string =  \"test\"\n")/utf8,
		("# Commentary 1\n")/utf8,
		("list_of_integers   = [1,\n2,\n\n3,4,5,\n\n\n6]\n")/utf8,
		("\n\nlist_of_atoms=[one   # Commentary 2\n ,two,\nthree\n]\n")/utf8,
		("# Commentary 3\n")/utf8,
		("integer  =  123456789 # Commentary 4\n")/utf8
	>>,
	{ok,Path} = file:get_cwd(),
	Full_path = lists:concat([Path,"/a_config.test"]),
	ok = file:write_file(Full_path,Config_source),
	{ok,Config} = parse_file(Full_path),
	ok = file:delete(Full_path),
	io:format("DONE! Configuration file parsing test passed.~n"),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_config) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Parse configuration file
-spec parse_file(Path) -> {ok,proplists:proplist()} | {error,Path}
	when
	Path :: a_unix_path_string().

parse_file(Path) ->
	{ok,Config_string} = clear_commentary(Path),
	case re:run(Config_string,"([a-z]{1}[a-zA-z0-9\_]{1,}\ {0,}\=)",[global]) of
		{match,Positions} -> parse_parameters(Positions,Config_string);
		_ -> {error,Path}
	end.


%% ----------------------------
%% @doc Parse parameters from configuration string
-spec parse_parameters(Positions,Config_string) -> {ok,Output}
	when
	Positions :: [[{pos_integer(),pos_integer()}]],
	Config_string :: a_utf_text_string(),
	Output :: a_utf_text_string().

parse_parameters(Positions,Config_string) ->
	parse_parameters_handler(Positions,Config_string,[]).


%% ----------------------------
%% @doc Parse parameter procedure handler
-spec parse_parameters_handler(Positions,Config_string,Output) -> {ok,Output}
	when
	Positions :: [[{pos_integer(),pos_integer()}]],
	Config_string :: a_utf_text_string(),
	Output :: a_utf_text_string().

parse_parameters_handler([],_,Output) -> {ok,Output};
parse_parameters_handler([[{A_begin,A_length},_]],Config_string,Output) ->
	parse_parameters_handler(
		[],ok,lists:append([Output,[{
			parse(string:sub_string(
				Config_string,A_begin,(A_begin+A_length-1)
			)),
			parse(string:sub_string(
				Config_string,(A_begin+A_length+1),length(Config_string)
			))
		}]])
	);
parse_parameters_handler([[{0,Length},_]|Positions],Config_string,Output) ->
	parse_parameters_handler(
		[[{1,Length},{}]|Positions],Config_string,Output
	);
parse_parameters_handler([[{A_begin,A_length},_],[{B_begin,B_length},_]|Positions],Config_string,Output) ->
	parse_parameters_handler(
		[[{B_begin,B_length},{}]|Positions],
		Config_string,
		lists:append([Output,[{
			parse(string:substr(
				Config_string,A_begin,A_length-1
			)),
			parse(string:sub_string(
				Config_string,(A_begin+A_length+1),(B_begin-1)
			))
		}]])
	).


%% ----------------------------
%% @doc Parse configuration term from string
-spec parse(String) -> term()
	when
	String :: a_utf_text_string().

parse(String) ->
	a_term:from_utf_string(
		lists:concat([String,"."])
	).


%% ----------------------------
%% @doc Wipe out commentary from configuration file
-spec clear_commentary(Config_path) -> {ok,a_utf_text_string()} | {error,_Reason}
	when
	Config_path :: file:io_device().

clear_commentary(Config_path) ->
	{ok,Config_file} = file:open(Config_path,read),
	clear_commentary_handler(Config_file,"").


%% ----------------------------
%% @doc Wipe out commentary from the configuration file line
-spec clear_commentary_handler(Config_file,Output) ->
	{ok,a_utf_text_string()} | {error,_Reason}
	when
	Config_file :: file:io_device(),
	Output :: a_utf_text_string().

clear_commentary_handler(Config_file,Output) ->
	case io:get_line(Config_file,'') of
		eof ->
			file:close(Config_file),
			{ok,Output};
		{error,Reason} ->
			{error,Reason};
		Line_income ->
			case re:run(Line_income,<<("#")/utf8>>) of
				{match,[{0,_}]} ->
					clear_commentary_handler(Config_file,Output);
				{match,[{Position,_}]} ->
					clear_commentary_handler(
						Config_file,
						lists:concat([Output,string:sub_string(
							unicode:characters_to_list(Line_income),
							1,Position-1
						)])
					);
				_ ->
					clear_commentary_handler(Config_file,lists:concat([
						Output,unicode:characters_to_list(Line_income)
					]))
			end
	end.