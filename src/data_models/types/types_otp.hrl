%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc OTP data types extention
%%%
%%% @end
%%% Created : 06. Май 2018 15:47
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

-type name_scope() :: {local,name_process()} | {global,name_process()} | {via,module(),name_process()}.
-type name_process() :: atom() | any().