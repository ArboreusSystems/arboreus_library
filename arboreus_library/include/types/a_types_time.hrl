%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Data types for Arboreus library: Time types
%%%
%%% @end
%%% Created : 08. Янв. 2018 18:33
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

-ifndef(A_TYPES_TIME).
-define(A_TYPES_TIME,1).

%% ------------------------------------------
%% Time types

-type a_time_tuple() :: {a_date(),a_time()}.
-type a_date() :: {a_year(),a_month(),a_day()}.
-type a_time() :: {a_hour(),a_minute(),a_second()}.
-type a_local_time() :: {a_date(),a_time()}.

-type a_time_unix_timestamp() :: pos_integer().
-type a_time_unix_nanoseconds() :: pos_integer().
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

-endif. %% A_TYPES_TIME