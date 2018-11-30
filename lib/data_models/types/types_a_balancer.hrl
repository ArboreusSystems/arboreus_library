%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus balancer data types definition
%%%
%%% @end
%%% Created : 06/19/2018 at 10:18
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% ----------------------------
%% @doc Arboreus balancer data models types

-type a_node_load() :: {
	a_node_load,
	anod_processes(),
	anod_ports(),
	anod_memory_total()
}.

-type anod_processes() :: 0 | pos_integer().
-type anod_ports() :: 0 | pos_integer().
-type anod_memory_total() :: 0 | pos_integer().
