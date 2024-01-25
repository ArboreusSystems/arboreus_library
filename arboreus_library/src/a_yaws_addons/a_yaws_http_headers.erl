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
-include("../../include/types/types_a_general.hrl").
-include("../../include/types/types_a_time.hrl").
-include("../../include/types/types_a_network.hrl").
-include("../../include/types/types_a_yaws.hrl").

%% Module API
-export([
	test/0,
	access_control_allow_origin/1,
	access_control_allow_methods/1,
	content_type/1,
	content_disposition/1,
	pragma/1,
	cache_control/1,
	last_modified/1,
	expires/1,
	cache/1,
	json/2,
	csv/2,
	xml/2,
	cors/0,cors/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Return HTTP Yaws header Access-Control-Allow-Origin within DOMAIN value
-spec access_control_allow_origin(DOMAIN) -> YAWS_HEADER
	when
		DOMAIN :: a_host_name_binary() | a_host_name_string() | a_ipv4_binary() | a_ipv4_string(),
		YAWS_HEADER :: a_yaws_http_header().

access_control_allow_origin(DOMAIN) when is_list(DOMAIN)->

	{header,["Access-Control-Allow-Origin:",DOMAIN]};

access_control_allow_origin(DOMAIN) ->

	access_control_allow_origin(a_var:to_string(DOMAIN)).


%% ----------------------------
%% @doc Return HTTP Yaws header Access-Control-Allow-Origin within DOMAIN value
-spec access_control_allow_methods(DOMAIN) -> YAWS_HEADER
	when
		DOMAIN :: a_host_name_binary() | a_host_name_string() | a_ipv4_binary() | a_ipv4_string(),
		YAWS_HEADER :: a_yaws_http_header().

access_control_allow_methods(DOMAIN) when is_list(DOMAIN)->

	{header,["Access-Control-Allow-Methods:",DOMAIN]};

access_control_allow_methods(DOMAIN) ->

	access_control_allow_origin(a_var:to_string(DOMAIN)).



%% ----------------------------
%% @doc Return HTTP Yaws header Content-Type within TYPE value
-spec content_type(TYPE) -> YAWS_HEADER
	when
		TYPE :: a_utf_text_string(),
		YAWS_HEADER :: a_yaws_http_header().

content_type(TYPE) when is_list(TYPE) ->

	{header,["Content-Type:",TYPE]};

content_type(_) ->

	{header,["Content-Type:","WrongContentType"]}.


%% ----------------------------
%% @doc Return HTTP Yaws header Content-Disposition within DISPOSITION value
-spec content_disposition(DISPOSITION) -> YAWS_HEADER
	when
		DISPOSITION :: a_utf_text_string(),
		YAWS_HEADER :: a_yaws_http_header().

content_disposition(TYPE) when is_list(TYPE) ->

	{header,["Content-Disposition:",TYPE]};

content_disposition(_) ->

	{header,["Content-Disposition:","WrongContentDisposition"]}.


%% ----------------------------
%% @doc Return HTTP Yaws header Pragma within PRAGMA_VALUE
-spec pragma(PRAGMA_VALUE) -> YAWS_HEADER
	when
		PRAGMA_VALUE :: a_utf_text_string(),
		YAWS_HEADER :: a_yaws_http_header().

pragma(PRAGMA_VALUE) when is_list(PRAGMA_VALUE) ->

	{header,["Pragma:",PRAGMA_VALUE]};

pragma(_) ->

	{header,["Pragma:","WrongPragma"]}.


%% ----------------------------
%% @doc Return HTTP Yaws header Cache-Control within CACHE value
-spec cache_control(CACHE) -> YAWS_HEADER
when
	CACHE :: a_utf_text_string(),
	YAWS_HEADER :: a_yaws_http_header().

cache_control(CACHE) when is_list(CACHE) ->

	{header,["Cache-Control:",CACHE]};

cache_control(_) ->

	{header,["Cache-Control:","WrongCacheControl"]}.


%% ----------------------------
%% @doc Wrapper function for cors/1, return default value for any domain.
-spec cors() -> YAWS_HEADERS
	when YAWS_HEADERS :: a_yaws_http_headers().

cors() -> cors("*").


%% ----------------------------
%% @doc Return list within header for Yaws Appmod
-spec cors(DOMAIN) -> YAWS_HEADERS
	when
		DOMAIN :: a_host_name_binary() | a_host_name_string() | a_ipv4_binary() | a_ipv4_string(),
		YAWS_HEADERS :: a_yaws_http_headers().

cors(DOMAIN) when is_list(DOMAIN) ->

	[
		access_control_allow_origin(DOMAIN),
		access_control_allow_methods(DOMAIN)
	];

cors(DOMAIN) ->

	cors(a_var:to_string(DOMAIN)).


%%-----------------------------------
%% @doc Return a list() within HTTP Header formated for Yaws out() function.
%% Example: "Last-Modified: Fri, 30 Oct 1998 14:19:41 GMT"
-spec last_modified(TIME) -> YAWS_HEADER
	when
		TIME :: pos_integer() | TIME_TUPLE | current,
		TIME_TUPLE :: {a_date(),a_time()},
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
		TIME :: pos_integer() | TIME_TUPLE | current,
		TIME_TUPLE :: {a_date(),a_time()},
		YAWS_HEADER :: a_yaws_http_header().

expires(TIME) when is_integer(TIME) == true, TIME > 0 ->

	{header,[
		"Expires:",
		a_var:to_string(a_time:format(
			rfc822,{timestamp_tuple,a_time:timestamp_to_tuple(TIME)})
		)
	]};

expires({{YEAR,MONTH,DAY},{HOUR,MINUTE,SECOND}})
	when
		is_integer(YEAR) == true, YEAR > 0,
		is_integer(MONTH) == true, MONTH > 0, MONTH =< 12,
		is_integer(DAY) == true, DAY > 0, DAY =< 31,
		is_integer(HOUR) == true, HOUR >= 0, HOUR =< 23,
		is_integer(MINUTE) == true, MINUTE >= 0, MINUTE =< 59,
		is_integer(SECOND) == true, SECOND >= 0, SECOND =< 59 ->

	{header,[
		"Expires:",
		a_var:to_string(a_time:format(
			rfc822,{date_tuple,{{YEAR,MONTH,DAY},{HOUR,MINUTE,SECOND}}})
		)
	]};

expires(current) -> expires(erlang:localtime()).


%%-----------------------------------
%% @doc Return a list() within HTTP headers fromated for Yaws out() function.
-spec cache(OPERATION) -> YAWS_HEADERS
	when
		OPERATION :: no,
		YAWS_HEADERS :: a_yaws_http_headers().

cache(no) ->

	[
		cache_control("no-cache, no-store, must-revalidate"),
		pragma("no-cache"),
		expires(1),
		last_modified(current)
	].


%%-----------------------------------
%% @doc Return a list within headers for JSON
-spec json(TYPE,FILE_NAME) -> YAWS_HEADERS
	when
		TYPE :: no_cache | solid,
		FILE_NAME :: unicode:latin1_chardata(),
		YAWS_HEADERS :: a_yaws_http_headers().

json(no_cache,FILE_NAME) ->

	[
		cache(no),
		json(solid,FILE_NAME)
	];

json(solid,FILE_NAME) ->

	[
		content_type("application/json; charset=utf-8"),
		content_disposition(lists:concat(["attachment; filename=",FILE_NAME]))
	].


%%-----------------------------------
%% @doc Return a list within HTTP headers for CSV file format
-spec csv(TYPE,FILE_NAME) -> YAWS_HEADERS
	when
		TYPE :: no_cache | solid,
		FILE_NAME :: unicode:latin1_chardata(),
		YAWS_HEADERS :: a_yaws_http_headers().

csv(no_cache,FILE_NAME) ->

	[
		cache(no),
		csv(solid,FILE_NAME)
	];

csv(solid,FILE_NAME) ->

	[
		content_type("text/csv; charset=utf-8"),
		content_disposition(lists:concat(["attachment; filename=",FILE_NAME]))
	].


%%-----------------------------------
%% @doc Return list within XML MIME type headers for Yaws out() function
-spec xml(TYPE,FILE_NAME) -> YAWS_HEADERS
	when
		TYPE :: no_cache | solid,
		FILE_NAME :: unicode:latin1_chardata(),
		YAWS_HEADERS :: a_yaws_http_headers().

xml(text,FILE_NAME) ->

	[
		content_type("text/xml; charset=utf-8"),
		content_disposition(lists:concat(["attachment; filename=",FILE_NAME]))
	];

xml(text_no_cache,FILE_NAME) ->

	[
		cache(no),
		xml(text,FILE_NAME)
	];

xml(application,FILE_NAME) ->

	[
		content_type("application/xml; charset=utf-8"),
		content_disposition(lists:concat(["attachment; filename=",FILE_NAME]))
	];

xml(application_no_cache,File_name) ->

	[
		cache(no),
		xml(application,File_name)
	].