%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc The records definition for the Arboreus cluster balancer
%%%
%%% @end
%%% Created : 06/19/2018 at 10:14
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").


%% ----------------------------
%% @doc Balancer data model definition

-record(a_node_load,{
	processes = 0 :: anod_processes(),
	ports = 0 :: anod_ports(),
	memory_total = 0 :: anod_memory_total()
}).