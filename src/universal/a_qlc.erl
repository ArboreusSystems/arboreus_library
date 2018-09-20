%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2016, http://arboreus.system
%%% @doc QLC module extention
%%%
%%% @end
%%% Created : 11. Февр. 2016 17:19
%%%-------------------------------------------------------------------
-module(a_qlc).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% System includes
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([
	select_ordered/3,select_ordered_local/2,
	select_paginated/5,select_paginated_local/4
]).


%%-----------------------------------
%% @doc Remote wrapper for select_ordered_local/2
-spec select_ordered(Node_name,Query,Order) -> {atomic,_Result} | {aborted,_Error_notice} | {error,_Unswear}
	when
	Node_name :: node(),
	Query :: qlc:query_handle(),
	Order :: fun().

select_ordered(Node_name,Query,Order) ->
	a_rpc:async_call(
		Node_name,?MODULE,select_ordered_local,[Query,Order]
	).


%%-----------------------------------
%% @doc Make request to Mnesia DB within ordering by defined field
-spec select_ordered_local(Query,Order) -> {atomic,_Result} | {aborted,_Error_notice} | {error,_Unswear}
	when
		Query :: qlc:query_handle(),
		Order :: fun().

select_ordered_local(Query,Order) ->
	mnesia:transaction(fun() ->
		qlc:eval(qlc:sort(Query,[{order,Order}]))
	end).


%%-----------------------------------
%% @doc Remote wrapper for select_paginated_local/4
-spec select_paginated(Node_name,Query,Order,From,To) -> {atomic,_Result} | {aborted,_Error_notice} | {error,_Unswear}
	when
	Node_name :: node(),
	Query :: qlc:query_handle(),
	Order :: fun(),
	From :: integer(),
	To :: integer().

select_paginated(Node_name,Query,Order,From,To) ->
	a_rpc:async_call(
		Node_name,?MODULE,select_paginated_local,[Query,Order,From,To]
	).


%%-----------------------------------
%% @doc Make request to Mnesia DB within ordering by defined field
%% and paginating by values From and To
-spec select_paginated_local(Query,Order,From,To) -> {atomic,_Result} | {aborted,_Error_notice} | {error,_Unswear}
	when
		Query :: qlc:query_handle(),
		Order :: fun(),
		From :: integer(),
		To :: integer().

select_paginated_local(Query,Order,From,To)
	when
	is_integer(From), is_integer(To),
	From >= 1, From < To ->
	mnesia:transaction(fun() ->
		Cursor = qlc:cursor(qlc:sort(Query,[{order,Order}])),
		if
			From == 1 ->
				Result = qlc:next_answers(Cursor,To),
				qlc:delete_cursor(Cursor),
				Result;
			true ->
				qlc:next_answers(Cursor,From-1),
				Result = qlc:next_answers(Cursor,To-From+1),
				qlc:delete_cursor(Cursor),
				Result
		end
	end).