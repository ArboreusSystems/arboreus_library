%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc The records definition for the Arboreus user handler
%%%
%%% @end
%%% Created : 06/03/2018 at 11:06
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% ----------------------------
%% @doc User data model definition

-record(a_user,{
	id :: a_user_id(),
	password :: a_user_password()
}).
-record(a_user_properties,{
	id :: a_user_id(),
	created :: a_user_create_time(),
	bd :: a_user_bd(),
	first_name :: a_user_first_name(),
	last_name :: a_user_last_name()
}).
-record(ause_login,{
	login :: a_user_login(),
	kind :: ause_login_kind_id(),
	user :: a_user_id()
}).
-record(ause_login_kind,{
	id :: ause_login_kind_id(),
	description :: ause_login_description(),
	rule :: ause_login_rule()
}).
