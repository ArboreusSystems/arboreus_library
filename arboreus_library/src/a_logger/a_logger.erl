%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 03. Jun 2024 18:52
%%%-------------------------------------------------------------------
-module(a_logger).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% Includes
-include_lib("a_includes.hrl").

%% API
-export([
	test/0,
	message_text/2
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Send message to logger process
-spec message_text(LOGGER_PID,MESSAGE) -> no_return()
	when
		LOGGER_PID :: pid(),
		MESSAGE :: a_utf_text_string().

message_text(LOGGER_PID,MESSAGE) ->

	gen_server:cast(LOGGER_PID,{write_text,MESSAGE}).
