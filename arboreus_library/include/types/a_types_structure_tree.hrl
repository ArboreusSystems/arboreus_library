%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Data types definition for a_structure
%%%
%%% @end
%%% Created : 28. Апр. 2018 11:40
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

-ifndef(A_TYPES_STRUCTURE_TREE).
-define(A_TYPES_STRUCTURE_TREE,1).

%% ----------------------------
%% @doc Structure data models definitions

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

-endif. %% A_TYPES_STRUCTURE_TREE