%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc The data types definitions
%%%
%%% @end
%%% Created : 10. Апр. 2018 14:32
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").


-type a_float_pos() :: float().
-type a_float_neg() :: float().

-type a_boolean_digit() :: 1 | 0.

-type a_unix_path() :: a_unix_path_binary() | a_unix_path_string().
-type a_unix_path_string() :: unicode:charlist().
-type a_unix_path_binary() :: unicode:unicode_binary().

-type a_utf_text() :: a_utf_text_binary() | a_utf_text_string().
-type a_utf_text_string() :: unicode:charlist().
-type a_utf_text_binary() :: unicode:unicode_binary().

-type a_utf_base64_text() :: a_utf_base64_string() | a_utf_base64_binary().
-type a_utf_base64_string() :: unicode:charlist().
-type a_utf_base64_binary() :: unicode:unicode_binary().

-type a_md() :: a_md5() | a_md4().
-type a_md_binary() :: a_md5_binary() | a_md4_binary().
-type a_md_string() :: a_md5_string() | a_md4_string().

-type a_md5() :: a_md5_binary() | a_md5_string().
-type a_md5_binary() :: <<_:32>>.
-type a_md5_string() :: a_utf_text_string().

-type a_md4() :: a_md5_binary() | a_md5_string().
-type a_md4_binary() :: <<_:32>>.
-type a_md4_string() :: a_utf_text_string().

-type a_id() :: a_id_8() | a_id_12() | a_id_16() | a_id_24() | a_id_32().
-type a_id_8() :: <<_:8>>.
-type a_id_12() :: <<_:12>>.
-type a_id_16() :: <<_:16>>.
-type a_id_24() :: <<_:25>>.
-type a_id_32() :: a_md5_binary() | a_md4_binary() | <<_:32>>.

-type a_record() :: tuple().
-type a_state() :: term().

-type a_list_numerated() :: [{pos_integer(),any()}].

-type a_list_of_options() :: a_list_of_properties().
-type a_list_of_properties() :: [any()].
-type a_list_of_parameters() :: [any()].
-type a_list_of_records() :: [a_record()].
-type a_list_of_integers() :: [integer()].
-type a_list_of_floats() :: [float()].
-type a_list_of_atoms() :: [atom()].
-type a_list_of_functions() :: [function()].
-type a_list_of_lists() :: [list()].
-type a_list_of_values() :: [any()].
-type a_list_of_tuples() :: [tuple()].
-type a_list_of_maps() :: [map()].
-type a_list_of_gb_trees() :: [gb_trees:tree()].
-type a_list_of_proplists() :: [proplists:proplist()].
-type a_list_of_numbers() :: [number()].

-type a_byte_8() :: 0..255.
-type a_byte_16() :: 0..16#ffffff.

-type a_year() :: 0..9999.
-type a_year_short() :: 0..99.
-type a_month() :: 1..12.
-type a_day() :: 1..31.
-type a_hour() :: 0..23.
-type a_minute() :: 0..59.
-type a_second() :: 0..59.

-type a_process_id() :: any().

-type a_size() :: integer().

-type a_file_path_string() :: a_utf_text_string().