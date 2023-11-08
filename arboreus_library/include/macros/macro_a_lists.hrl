%%%-------------------------------------------------------------------
%%% @author alexandr
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Nov 2023 18:18
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Definitions
-define(A_LIST_FROM_RECORD(RECORD),
	fun(VALUE) ->
		FIELDS = record_info(fields,RECORD),
		[_TAG|VALUES] = tuple_to_list(VALUE),
		lists:zip(FIELDS,VALUES)
	end
).