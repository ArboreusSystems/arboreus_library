%%%-------------------------------------------------------------------
%%% @author alexandr
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Nov 2023 21:07
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

-include("a_includes.hrl").

-ifndef(A_RECORDS_NODE).
-define(A_RECORDS_NODE,1).

%% ----------------------------
%% @doc

-record(a_node_start_properties,{

	name :: a_node_name_string(),
	host :: a_host_name_string(),
	detached :: boolean(),
	cookie :: a_node_cookie_string(),
	port_range :: boolean(),
	port_range_min :: port(),
	port_range_max :: port(),
	command_timeout :: integer(),
	shutdown_time :: integer()
}).

-endif. %% A_RECORDS_NODE