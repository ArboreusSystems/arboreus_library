%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2016, http://arboreus.system
%%% @doc Lpath (List path) handler
%%%
%%% @end
%%% Created : 06. Май 2016 21:19
%%%-------------------------------------------------------------------
-module(a_lpath).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% System include
-include_lib("../include/types/types_a_general.hrl").

%% API
-export([
	test/0,
	get/2,
	set/3
]).


% -------------------------------------------------------------------
% @doc Module test function
-spec test() -> ok.

test() -> ok.


% -------------------------------------------------------------------
% @doc Compute the Lpath from proplist
-spec get(Lpath,Proplist) -> any() | false
	when
	Lpath :: list(),
	Proplist :: proplists:proplist().

get([],Value) -> {ok,Value};
get([Point|Path],Proplist) ->
	case proplists:get_value(Point,Proplist) of
		undefined -> {error,wrong_path};
		Value -> get(Path,Value)
	end.


% -------------------------------------------------------------------
% @doc Set new pair by passing through the defined path
-spec set(Value,Path,Proplist) -> proplists:proplist() | {error,_Reason}
	when
	Value :: any(),
	Path :: list(),
	Proplist :: proplists:proplist().

set(Value,Path,Proplist) ->
	case expand(Path,Proplist,[]) of
		{error,Reason} -> {error,Reason};
		Proplist_cache ->
			compose(
				lists:reverse(Proplist_cache),
				{lists:last(Path),Value},
				lists:reverse(Path)
			)
	end.


% -------------------------------------------------------------------
% @doc Condense the original proplist within included new pair
-spec compose(Cache_list,Pair,Reversed_path) -> proplists:proplist()
	when
	Cache_list :: list(),
	Pair :: tuple(),
	Reversed_path :: list().

compose([],Proplist,[]) -> Proplist;
compose([List|Cache_list],Pair,[Point|Path]) ->
	compose(
		Cache_list,
		lists:append(List,[{Point,Pair}]),
		Path
	).


% -------------------------------------------------------------------
% @doc Make separated by path cache of list, secondary function for set/3
-spec expand(Path,Proplist,Cache_list) -> list() | {error,_Reason}
	when
	Path :: list(),
	Proplist :: proplists:proplist(),
	Cache_list :: list().

expand([],_,List_cache) -> List_cache;
expand([Point|Path],Proplist,List_cache) ->
	case proplists:get_value(Point,Proplist) of
		undefined -> {error,wrong_path};
		Point_value ->
			List_cache_out = lists:append(
				List_cache,
				[proplists:delete(Point,Proplist)]
			),
			expand(Path,Point_value,List_cache_out)
	end.