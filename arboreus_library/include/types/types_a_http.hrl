%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc The data types definitions for the POST requests
%%%
%%% @end
%%% Created : 10. Апр. 2018 16:57
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").


%% ------------------------------------------
%% HTTP request types

-type a_http_post_parameter() :: a_utf_text_string().
-type a_http_url() :: unicode:latin1_chardata().
-type a_http_json_binary() :: unicode:unicode_binary().