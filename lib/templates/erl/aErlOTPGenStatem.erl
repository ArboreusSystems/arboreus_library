%%% -------------------------------------------------------------------
%%% @doc
%%% @notice
%%%
%%% @copyright Arboreus (http://arboreus.systems)
%%% @author Alexandr Kirilov (http://alexandr.kirilov.me)
%%% @created 01/25/2019 at 15:29
%%% -------------------------------------------------------------------
-module(aErlOTPGenStatem).
-author("Alexandr Kirilov (http://alexandr.kirilov.me)").
-behaviour(gen_statem).

%% Constants
-define(SERVER, ?MODULE).

%% Data types

%% Data models
-record(state, {}).

%% API
-export([
	
	%% Test functionality
	test/0,
	
	%% Module functionality
	start_link/0,
	init/1,
	terminate/3,
	code_change/4,
	callback_mode/0,
	
	%% Gen_statem functionlity
	format_status/2,
	state_name/3,
	handle_event/4

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (aErlOTPGenStatem) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (aErlOTPGenStatem) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%%--------------------------------------------------------------------
%% @doc Start gen_fsm process
-spec start_link() -> {ok, Pid} | ignore | {error, Error}.

start_link() ->
	gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc Initiate gen_fsm process
-spec init(Args) ->
	{CallbackMode, StateName, State} |
	{CallbackMode, StateName, State, Actions} |
	ignore |
	{stop, StopReason}.

init([]) ->
	{ok, state_name, #state{}}.


%%--------------------------------------------------------------------
%% @doc Set callback mode
-spec callback_mode() -> atom().

callback_mode() ->
	handle_event_function.


%%--------------------------------------------------------------------
%% @doc Ser the status format (optional)
-spec format_status(Opt, [PDict|StateName|State]) -> term().

format_status(_Opt, [_PDict, _StateName, _State]) ->
	Status = some_term,
	Status.


%%--------------------------------------------------------------------
%% @doc Set state name
-spec state_name(Event, From, State) ->
	{next_state, NextStateName, NextState} |
	{next_state, NextStateName, NextState, Actions} |
	{stop, Reason, NewState} |
	stop |
	{stop, Reason :: term()} |
	{stop, Reason :: term(), NewData :: data()} |
	{stop_and_reply, Reason, Replies} |
	{stop_and_reply, Reason, Replies, NewState} |
	{keep_state, NewData :: data()} |
	{keep_state, NewState, Actions} |
	keep_state_and_data |
	{keep_state_and_data, Actions}.

state_name(_EventType, _EventContent, State) ->
	NextStateName = next_state,
	{next_state, NextStateName, State}.


%%--------------------------------------------------------------------
%% @doc Message handler
-spec handle_event(EventType, Event, StateName, State) ->
	{next_state, NextStateName, NextState} |
	{next_state, NextStateName, NextState, Actions} |
	{stop, Reason, NewState} |
	stop |
	{stop, Reason :: term()} |
	{stop, Reason :: term(), NewData :: data()} |
	{stop_and_reply, Reason, Replies} |
	{stop_and_reply, Reason, Replies, NewState} |
	{keep_state, NewData :: data()} |
	{keep_state, NewState, Actions} |
	keep_state_and_data |
	{keep_state_and_data, Actions}.

handle_event(_EventType, _EventContent, _StateName, State) ->
	NextStateName = the_next_state_name,
	{next_state, NextStateName, State}.


%%--------------------------------------------------------------------
%% @doc Terminate gen_statem
-spec terminate(
		Reason :: normal | shutdown | {shutdown, term()} | term(),
		StateName :: atom(),
		StateData :: term()) -> term().

terminate(_Reason, _StateName, _State) ->
	ok.


%%--------------------------------------------------------------------
%% @doc Change code
-spec code_change(
		OldVsn :: term() | {down, term()},
		StateName :: atom(),
		StateData :: #state{},
		Extra :: term()
) ->
	{ok, StateName, StateData}.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.
