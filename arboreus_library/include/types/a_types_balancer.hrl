%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus balancer data types definition
%%%
%%% @end
%%% Created : 06/19/2018 at 10:18
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

-ifndef(A_TYPES_BALANCER).
-define(A_TYPES_BALANCER,1).

%% ----------------------------
%% @doc Arboreus balancer data models types

-type anod_processes() :: 0 | pos_integer().
-type anod_ports() :: 0 | pos_integer().
-type anod_memory_total() :: 0 | pos_integer().

-endif. %% A_TYPES_BALANCER
