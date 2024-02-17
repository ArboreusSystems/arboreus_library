%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2024 12:18
%%%-------------------------------------------------------------------
-module(a_record).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% API
-export([
	test/0,
	name/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Return record name
-spec name(RECORD) -> {ok,RECORD_NAME} | {error,REASON}
	when
		RECORD :: term(),
		RECORD_NAME :: atom(),
		REASON :: term().

name(RECORD) when is_tuple(RECORD)->

	RECORD_NAME = element(1,RECORD),
	case is_atom(RECORD_NAME) of
		true -> {ok,RECORD_NAME};
		_ -> {error,[{"RECORD",false}]}
	end;

name(_) -> {error,[{"RECORD",false}]}.
