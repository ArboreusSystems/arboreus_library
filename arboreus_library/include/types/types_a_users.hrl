%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus users data types definition
%%%
%%% @end
%%% Created : 06/03/2018 at 11:06
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").


%% ----------------------------
%% @doc Arboreus users data models types

-type a_user() :: {
	a_user,
	a_user_id(),
	a_user_password()
}.
-type a_user_properties() :: {
	a_user_properties,
	a_user_id(),
	a_user_create_time(),
	a_user_bd(),
	a_user_first_name(),
	a_user_last_name()
}.
-type ause_login() :: {
	ause_login,
	a_user_login(),
	ause_login_kind_id(),
	a_user_id()
}.
-type ause_login_kind() :: {
	ause_login_kind,
	ause_login_kind_id(),
	ause_login_description(),
	ause_login_rule()
}.


%% ----------------------------
%% @doc Arboreus users data primitives types

-type a_user_id() :: a_id_12().
-type a_user_password() :: a_md5_binary().
-type a_user_login() :: a_utf_text_binary().
-type a_user_create_time() :: a_time_unix_timestamp().
-type a_user_bd() :: pos_integer().
-type a_user_first_name() :: a_utf_text_binary().
-type a_user_last_name() :: a_utf_text_binary().
-type ause_login_kind_id() :: pos_integer().
-type ause_login_description() :: a_utf_text_binary().
-type ause_login_rule() :: a_utf_text_binary().