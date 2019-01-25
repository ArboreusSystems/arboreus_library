%%% -------------------------------------------------------------------
%%% @doc
%%% @notice
%%%
%%% @copyright Arboreus (http://arboreus.systems)
%%% @author Alexandr Kirilov (http://alexandr.kirilov.me)
%%% @created 01/25/2019 at 13:45
%%% -------------------------------------------------------------------
{application, aErlOTPApplication, [
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