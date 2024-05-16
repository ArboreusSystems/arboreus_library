%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc The records definition for the structures handler
%%%
%%% @end
%%% Created : 28. Апр. 2018 11:28
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

-ifndef(A_RECORDS_STRUCTURE_TREE).
-define(A_RECORDS_STRUCTURE_TREE,1).

%% ----------------------------
%% @doc Records for defining tree-like structures

-record(astr_point,{

	id :: astr_point_id(),
	weight = 0 :: astr_point_weight(),
	twig = 0 :: astr_twig_id(),
	kind :: astr_point_kind(),
	container :: astr_point_container()
}).


%% ----------------------------
%% @doc

-record(astr_link,{

	id :: astr_link_id(),
	point_a :: astr_point_id(),
	point_b :: astr_point_id(),
	strength = 0 :: astr_link_strength()
}).


%% ----------------------------
%% @doc

-record(astr_alias,{

	alias :: #astr_alias{},
	point :: astr_point_id(),
	description :: astr_alias_description()
}).


%% ----------------------------
%% @doc

-record(astr_twig,{

	twig :: astr_twig_id(),
	description :: astr_twig_description()
}).

-endif. %% A_RECORDS_STRUCTURE_TREE