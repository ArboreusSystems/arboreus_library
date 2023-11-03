%%%-------------------------------------------------------------------
%%% @author alexandr
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Nov 2023 21:12
%%%-------------------------------------------------------------------
-module(a_error).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% API
-export([
	nif_not_loaded/2
]).


%% ----------------------------
%% @doc Function wrapper for erlang:nif_error/1
-spec nif_not_loaded(MODULE,LINE) -> no_return()
	when
		MODULE :: atom(),
		LINE :: number().

nif_not_loaded(MODULE,LINE) ->

	erlang:nif_error({not_loaded, [{module,MODULE}, {line,LINE}]}).
