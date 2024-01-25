%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV (http://alexandr.kirilov.me)
%%% @copyright (C) 2015, Arboreus, (http://arboreus.systems)
%%% @doc Yaws appmode reply headers set generator
%%%
%%% @end
%%% Created : 08. Aug 2015 18:48
%%%-------------------------------------------------------------------
-module(a_yaws_http_headers).
-author("Alexandr KIRILOV (http://alexandr.kirilov.me)").

%% System include

%% Application includes
-include("../../include/types/types_a_yaws.hrl").

%% Module API
-export([
	test/0,
	last_modified/1,
	expires/1,
	cache/1,
	json/2,
	csv/2,
	xml/2,
	cross_domain/0,cross_domain/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Wrapper function for cross_domain/1, return default value for any domain.
-spec cross_domain() -> list().

cross_domain() -> cross_domain("*").


%% ----------------------------
%% @doc Return list within header for Yaws Appmod
-spec cross_domain(DOMAIN) -> YAWS_HEADER
	when
		DOMAIN :: string() | binary() | atom() | integer() | float(),
		YAWS_HEADER :: a_yaws_http_header().

cross_domain(DOMAIN) when is_list(DOMAIN) ->

	{header,["Access-Control-Allow-Origin:",DOMAIN]};

cross_domain(DOMAIN) ->

	cross_domain(a_var:to_string(DOMAIN)).


%%-----------------------------------
%% @doc Return a list() within HTTP Header formated for Yaws out() function.
%% Example: "Last-Modified: Fri, 30 Oct 1998 14:19:41 GMT"
-spec last_modified(TIME) -> YAWS_HEADER
	when
		TIME_TUPLE :: {{YEAR,MONTH,DAY},{HOUR,MINUTE,SECOND}},
		YEAR :: pos_integer(),
		MONTH :: 1..12,
		DAY :: 1..31,
		HOUR :: 0..23,
		MINUTE :: 0..59,
		SECOND :: 0..59,
		TIME :: pos_integer() | TIME_TUPLE | current,
		YAWS_HEADER :: a_yaws_http_header().

last_modified(TIME) when is_integer(TIME), TIME > 0 ->

	{header,[
		"Last-Modified:",
		a_var:to_string(a_time:format(
			rfc822,{timestamp_tuple,a_time:timestamp_to_tuple(TIME)})
		)
	]};

last_modified({{YEAR,MONTH,DAY},{HOUR,MINUTE,SECOND}})
	when
		is_integer(YEAR) == true, YEAR > 0,
		is_integer(MONTH) == true, MONTH > 0, MONTH =< 12,
		is_integer(DAY) == true, DAY > 0, DAY =< 31,
		is_integer(HOUR) == true, HOUR >= 0, HOUR =< 23,
		is_integer(MINUTE) == true, MINUTE >= 0, MINUTE =< 59,
		is_integer(SECOND) == true, SECOND >= 0, SECOND =< 59 ->

	{header,[
		"Last-Modified:",
		a_var:to_string(a_time:format(
			rfc822,{date_tuple,{{YEAR,MONTH,DAY},{HOUR,MINUTE,SECOND}}})
		)
	]};

last_modified(current) ->

	last_modified(erlang:localtime()).


%%-----------------------------------
%% @doc Return a list() within HTTP Header formated for Yaws out() function.
%% Example: "Expires: Fri, 30 Oct 1998 14:19:41 GMT"
-spec expires(TIME) -> YAWS_HEADER
	when
		TIME_TUPLE :: {{YEAR,MONTH,DAY},{HOUR,MINUTE,SECOND}},
		YEAR :: pos_integer(),
		MONTH :: pos_integer(),
		DAY :: pos_integer(),
		HOUR :: pos_integer(),
		MINUTE :: pos_integer(),
		SECOND :: pos_integer(),
		TIME :: pos_integer() | TIME_TUPLE | current,
		YAWS_HEADER :: a_yaws_http_header().

expires(Timestamp) when is_integer(Timestamp) == true, Timestamp > 0 ->

	{header,[
		"Expires:",
		a_var:to_string(a_time:format(
			rfc822,{timestamp_tuple,a_time:timestamp_to_tuple(Timestamp)})
		)
	]};

expires({{Year,Month,Day},{Hour,Minute,Second}})
	when
		is_integer(Year) == true, Year > 0,
		is_integer(Month) == true, Month > 0, Month =< 12,
		is_integer(Day) == true, Day > 0, Day =< 31,
		is_integer(Hour) == true, Hour >= 0, Hour =< 23,
		is_integer(Minute) == true, Minute >= 0, Minute =< 59,
		is_integer(Second) == true, Second >= 0, Second =< 59 ->

	{header,[
		"Expires:",
		a_var:to_string(a_time:format(
			rfc822,{date_tuple,{{Year,Month,Day},{Hour,Minute,Second}}})
		)
	]};

expires(current) -> expires(erlang:localtime()).


%%-----------------------------------
%% @doc Return a list() within HTTP headers fromated for Yaws out() function.
-spec cache(Operation) -> list()
	when
	Operation :: no.

cache(no) ->
	[
		{header,"Cache-Control: no-cache, no-store, must-revalidate"},
		{header,"Pragma: no-cache"},
		expires(1),
		last_modified(current)
	].


%%-----------------------------------
%% @doc Return a list within headers for JSON
-spec json(Type,File_name) -> list()
	when
		Type :: no_cache | solid,
		File_name :: unicode:latin1_chardata().

json(no_cache,File_name) ->
	[
		cache(no),
		json(solid,File_name)
	];
json(solid,File_name) ->
	[
		{header,["Content-Type:","application/json; charset=utf-8"]},
		{header,["Content-Disposition:",lists:concat(["attachment; filename=",File_name])]}
	].


%%-----------------------------------
%% @doc Return a list within HTTP headers for CSV file format
-spec csv(Type,File_name) -> list() | {error,_Reason}
	when
		Type :: no_cache | solid,
		File_name :: unicode:latin1_chardata().

csv(no_cache,File_name) ->
	[
		cache(no),
		csv(solid,File_name)
	];
csv(solid,File_name) ->
	[
		{header,["Content-Type:","text/csv; charset=utf-8"]},
		{header,["Content-Disposition:",lists:concat(["attachment; filename=",File_name])]}
	].


%%-----------------------------------
%% @doc Return list within XML MIME type headers for Yaws out() function
-spec xml(Content_type,File_name) -> list() | {error,_Reason}
	when
		Content_type :: text_xml | application_xml,
		File_name :: unicode:latin1_chardata().

xml(text,File_name) ->
	[
		{header,["Content-Type:","text/xml; charset=utf-8"]},
		{header,["Content-Disposition:",lists:concat(["attachment; filename=",File_name])]}
	];
xml(text_no_cache,File_name) -> [cache(no),xml(text,File_name)];
xml(application,File_name) ->
	[
		{header,["Content-Type:","application/xml; charset=utf-8"]},
		{header,["Content-Disposition:",lists:concat(["attachment; filename=",File_name])]}
	];
xml(application_no_cache,File_name) ->
	[cache(no),xml(application,File_name)].