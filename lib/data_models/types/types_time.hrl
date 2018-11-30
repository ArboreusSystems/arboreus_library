%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Data types for Arboreus library: Time types
%%%
%%% @end
%%% Created : 08. Янв. 2018 18:33
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").


%% ------------------------------------------
%% Time types

-type a_time_tuple() :: {a_date(),a_time()}.
-type a_date() :: {year(),month(),day()}.
-type a_time() :: {hour(),minute(),second()}.
-type local_time() :: {a_date(),a_time()}.
-type a_timestamp() :: pos_integer().
-type a_date_integer() :: pos_integer().
