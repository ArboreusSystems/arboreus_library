%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 25. Jan 2024 12:28
%%%-------------------------------------------------------------------
-author("Alexandr Kirilov, https://alexandr.kirilov.me").


%% ----------------------------
%% @doc Yaws Web Server data types

-type a_yaws_arguments() :: a_record().
-type a_yaws_http_header_name() :: string().
-type a_yaws_http_header_value() :: string().
-type a_yaws_http_header() :: {atom(),list()}.
-type a_yaws_http_headers() :: [a_yaws_http_header()].
-type a_yaws_appmode_output() :: list().
-type a_yaws_post_parameters() :: proplists:proplist().