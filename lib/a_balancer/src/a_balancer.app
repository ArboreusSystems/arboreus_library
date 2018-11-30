%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus balancer application resource file
%%%
%%% @end
%%% Created : 06/06/2018 at 10:15
%%%-------------------------------------------------------------------
{application, a_balancer, [
	{description, "OTP application"},
	{vsn, "1"},
	{registered, []},
	{applications, [
		kernel,
		stdlib
	]},
	{start_phases, [{phase1, []}, {phase2, []}, {phase3, []}]},
	{mod, {a_balancer, []}},
	{env, []}
]}.