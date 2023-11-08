%%%-------------------------------------------------------------------
%%% @author alexandr
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Nov 2023 21:07
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").


%% ----------------------------
%% @doc

-record(a_node_properties,{

	name :: node_name_string(),
	host :: host_name_string(),
	detached :: boolean(),
	cookie :: cookie_string(),
	port_range :: boolean(),
	port_range_min :: port(),
	port_range_max :: port(),
	command_timeout :: integer()
}).