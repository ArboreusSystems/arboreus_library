%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2016, http://arboreus.system
%%% @doc The HTML stub reply for Yaws appmode
%%%
%%% @end
%%% Created : 06. Янв. 2016 13:22
%%%-------------------------------------------------------------------
-module(a_stub).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

-define(STUB_HEADER,"Arboreus stub page").
-define(STUB_TEXT,"You see this page because requested function is stubbed").

%% API
-export([
	test/0,
	set/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc return a list within prepeared stub page for Yaws appmod
-spec set(Type) -> list()
	when
		Type :: html | http_headers.

set(html) ->
	[
		a_http_headers:cache(no),
		{'ehtml',[
			["<!DOCTYPE html> "],
			{'html',[],[
				{'head',[],[
					{'title',[],?STUB_HEADER},
					["\n"]
				]},
				{'h1',[],?STUB_HEADER},
				{'div',[],?STUB_TEXT},
				["<br>"],
				{'div',[],[{'a',[{'href',"http://arboreus.systems/"}],"Arboreus Library (C) 2014-2019"}]}
			]}
		]}
	];
set(http_headers) ->
	[
		a_http_headers:cache(no),
		{header,["Stub:","yes"]},
		{header,["Type:",?STUB_HEADER]},
		{header,["Note:",?STUB_TEXT]},
		{header,["Link:","http://arboreus.systems/"]}
	];
set(_) ->
	[
		a_http_headers:cache(no),
		{header,["Stub_error:","wrong stub type"]}
	].