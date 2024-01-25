%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Data types definition for a_structure
%%%
%%% @end
%%% Created : 28. Апр. 2018 11:40
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").


%% ----------------------------
%% @doc Structure data models definitions

-type astr_point() :: {
	astr_point,
	astr_point_id(),
	astr_point_weight(),
	astr_twig_id(),
	astr_point_kind(),
	astr_point_container()
}.
-type astr_link() :: {
	astr_link,
	astr_link_id(),
	astr_point_id(),
	astr_point_id(),
	astr_link_strength()
}.
-type astr_alias() :: {
	astr_alias,
	astr_alias_id(),
	astr_point_id(),
	astr_alias_description()
}.
-type astr_twig() :: {
	astr_twig,
	astr_twig_id(),
	astr_twig_description()
}.

-type astr_point_id() :: <<_:32>>.
-type astr_point_weight() :: 0 | integer().
-type astr_point_container() :: any().
-type astr_point_kind() :: any().
-type astr_link_strength() :: 0 | integer().
-type astr_link_id() :: a_md5_binary().
-type astr_link_points() :: [astr_point_id()|astr_point_id()].
-type astr_alias_id() :: any().
-type astr_alias_description() :: a_utf_text_binary().
-type astr_twig_id() :: any().
-type astr_twig_description() :: a_utf_text_binary().