%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 24. Dec 2024 19:19
%%%-------------------------------------------------------------------
-module(a_json).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

%% System includes

%% Application includes
-include("a_includes.hrl").

%% API
-export([

	test/0,

	encode/1

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Encode JSON from Erlang value
-spec encode(DATA) -> OUTPUT
	when
		DATA :: any(),
		OUTPUT :: a_utf_text_string().

%% Multiverse functionality for encode/1 function
-if(?OTP_RELEASE == 25).
-define(A_JSON_ENCODE(IN_DATA),json2:encode(IN_DATA)).
-elif(?OTP_RELEASE == 27).
-define(A_JSON_ENCODE(IN_DATA),json:encode(IN_DATA)).
-else.
-define(A_JSON_ENCODE(IN_DATA),a_string:from_term({error,{otp_release,IN_DATA}})).
-endif.

encode(DATA) -> ?A_JSON_ENCODE(DATA).