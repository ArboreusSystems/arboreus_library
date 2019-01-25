%%% -------------------------------------------------------------------
%%% @doc
%%% @notice
%%%
%%% @copyright Arboreus (http://arboreus.systems)
%%% @author Alexandr Kirilov (http://alexandr.kirilov.me)
%%% @created 01/25/2019 at 15:07
%%% -------------------------------------------------------------------
-module(aErlOTPSupervisor).
-author("Alexandr Kirilov (http://alexandr.kirilov.me)").
-behaviour(supervisor).

%% Constants
-define(SERVER, ?MODULE).
-define(SV_STRATEGY, one_for_one).
-define(SV_INTENSITY, 1000).
-define(SV_PERIOD, 3600).
-define(CHILD_START(Module), {Module, start_link, []}).
-define(CHILD_RESTART, permanent).
-define(CHILD_SHUTDOWN, 2000).
-define(CHILD_TYPE, worker).
-define(CHILD_MODULES, dynamic).

%% Data types

%% Data models

%% API
-export([
	
	%% Test module functionality
	test/0,
	
	%% Process start and stop
	start_link/0,
	stop/0,
	init/1,
	
	%% Children's handlers
	child/1, child/2,
	count_children/0,
	erase_child/1, erase_child/2

]).


%% ----------------------------
%% @doc Module test function 
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (aErlOTPSupervisor) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (aErlOTPSupervisor) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Initial supervisor strategy
-spec init(Arguments) -> {ok, {Sup_flags, [Child_spec]}} | ignore | {error, Reason}
	when
	Arguments :: term(),
	Sup_flags :: map(),
	Reason :: term(),
	Child_spec :: map().

init([]) ->
	{ok, {#{
		strategy => ?SV_STRATEGY,
		intensity => ?SV_INTENSITY,
		period => ?SV_PERIOD
	}, [
		%% List of children 		
	]}}.


%% ----------------------------
%% @doc Supervisor start link
-spec start_link() -> {ok, Pid} | ignore | {error, Reason}
	when
	Pid :: pid(),
	Reason :: term().

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%% ----------------------------
%% @doc The supervisor stopping functionality
-spec stop() -> true.

stop() -> exit(whereis(?MODULE), shutdown).


%% ----------------------------
%% @doc The child specification setup
-spec child(Module) -> map()
	when
	Module :: module().

child(Module) -> child(Module, []).


%% ----------------------------
%% @doc The child specification setup handler
-spec child(Module, Properties) -> map()
	when
	Module :: module(),
	Properties :: proplists:proplist().

child(Module, Properties) ->
	#{
		id => Module,
		start => case proplists:get_value(start, Properties) of
			undefined -> ?CHILD_START(Module);
			Value_start -> Value_start
		end,
		restart => case proplists:get_value(restart, Properties) of
			undefined -> ?CHILD_RESTART;
			Value_restart -> Value_restart
		end,
		shutdown => case proplists:get_value(shutdown, Properties) of
			undefined -> ?CHILD_SHUTDOWN;
			Value_shutdown -> Value_shutdown
		end,
		type => case proplists:get_value(type, Properties) of
			undefined -> ?CHILD_TYPE;
			Value_type -> Value_type
		end,
		modules => case proplists:get_value(modules, Properties) of
			undefined -> ?CHILD_MODULES;
			Value_modules -> Value_modules
		end
	}.


%% ----------------------------
%% @doc The proxy functionality to the supervisor module, counting childrens
-spec count_children() -> proplists:proplist().

count_children() -> supervisor:count_children(?SERVER).


%% ----------------------------
%% @doc Erase child from supervisor tree, do the termination and deleting of child in one step
-spec erase_child(Child) -> ok
	when
	Child :: supervisor:child_id().

erase_child(Child) -> erase_child(?SERVER, Child).


%% ----------------------------
%% @doc Erase child from supervisor tree, do the termination and deleting of child in one step
-spec erase_child(Process_reference, Child) -> ok
	when
	Process_reference :: pid() | atom,
	Child :: supervisor:child_id().

erase_child(Process_reference, Child) ->
	ok = supervisor:terminate_child(Process_reference, Child),
	supervisor:delete_child(Process_reference, Child).