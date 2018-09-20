%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc The Arboreus templates: OTP Application resource file
%%%
%%% @end
%%% Created : 06/01/2018 at 16:14
%%%-------------------------------------------------------------------
{application, otp_application, [
	{description, "OTP application"},
	{vsn, "1"},
	{registered, []},
	{applications, [
		kernel,
		stdlib
	]},
	{start_phases, [{phase1, []}, {phase2, []}, {phase3, []}]},
	{mod, {otp_application, []}},
	{env, []}
]}.