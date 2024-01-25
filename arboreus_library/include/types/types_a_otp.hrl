%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc OTP data types extention
%%%
%%% @end
%%% Created : 06. Май 2018 15:47
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

-type a_name_scope() :: {local,a_name_process()} | {global,a_name_process()} | {via,module(),a_name_process()}.
-type a_name_process() :: atom() | any().