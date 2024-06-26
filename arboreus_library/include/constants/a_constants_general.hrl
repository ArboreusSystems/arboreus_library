%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc General constants for library
%%%
%%% @end
%%% Created : 10. Апр. 2018 15:54
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

-ifndef(A_CONSTANTS_GENERAL).
-define(A_CONSTANTS_GENERAL,1).

-define(APPLICATION_NAME,"Arboreus Library").
-define(APPLICATION_HEADER_OK,{header,["Appplication:","ok"]}).
-define(APPLICATION_HEADER_ERROR,fun(X) -> {header,["Appplication:",X]} end).

-endif. %% A_CONSTANTS_GENERAL