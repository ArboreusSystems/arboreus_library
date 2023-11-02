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

-type a_time_unix_timestamp() :: pos_integer().
-type a_time_unix_microseconds() :: pos_integer().
-type a_time_unix_milliseconds() :: pos_integer().
-type a_time_unix_seconds() :: pos_integer().

-type a_time_integer() :: pos_integer().
-type a_time_integer_date() :: pos_integer().
-type a_time_integer_full() :: pos_integer().
-type a_time_integer_extend() :: pos_integer().

-type a_time_rfc822() :: string().
-type a_time_rfc850() :: string().
-type a_time_ansi() :: string().