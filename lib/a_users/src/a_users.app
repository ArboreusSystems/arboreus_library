%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus user handler application resource file
%%%
%%% @end
%%% Created : 06/03/2018 at 11:09
%%%-------------------------------------------------------------------
{application, a_users, [
	{description, "Arboreus user handler"},
	{vsn, "1"},
	{registered, []},
	{applications, [
		kernel,
		stdlib,
		mnesia
	]},
	{start_phases, [{phase1, []}, {phase2, []}, {phase3, []}]},
	{mod, {a_users, []}},
	{env, []}
]}.