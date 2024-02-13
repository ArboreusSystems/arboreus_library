%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 11. Feb 2024 17:00
%%%-------------------------------------------------------------------
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

-record(a_properties_state,{

	storage_id :: atom(),
	storage_tid :: ets:tid()
}).

-record(a_properties_pair,{

	key :: atom() | a_utf_text_binary() | a_utf_text_string(),
	value :: any()
}).