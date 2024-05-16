%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc OTP data types extention
%%%
%%% @end
%%% Created : 06. Май 2018 15:47
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

-ifndef(A_TYPES_OTP).
-define(A_TYPES_OTP,1).

-type a_name_scope() :: {local,a_name_process()} | {global,a_name_process()} | {via,module(),a_name_process()}.
-type a_name_process() :: atom() | any().

-endif. %% A_TYPES_OTP