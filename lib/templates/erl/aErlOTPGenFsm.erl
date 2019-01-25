%%% -------------------------------------------------------------------
%%% @doc
%%% @notice
%%%
%%% @copyright Arboreus (http://arboreus.systems)
%%% @author Alexandr Kirilov (http://alexandr.kirilov.me)
%%% @created 01/25/2019 at 15:00
%%% -------------------------------------------------------------------
-module(aErlOTPGenFsm).
-author("Alexandr Kirilov (http://alexandr.kirilov.me)").
-behaviour(gen_fsm).

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
	init/1,
	start_link/0,
	code_change/4,
	terminate/3,
	
	%% Application functionality
	
	%% Gen_FSM functinolity
	state_name/2,
	state_name/3,
	handle_event/3,
	handle_sync_event/4,
	handle_info/3

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (aErlModule) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (aErlModule) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%%--------------------------------------------------------------------
%% @doc Start gen_fsm process
-spec start_link() -> {ok, pid()} | ignore | {error, Reason :: term()}.

start_link() ->
	gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc Initiate gen_fsm process
-spec init(Args :: term()) ->
	{ok, StateName :: atom(), StateData :: #state{}} |
	{ok, StateName :: atom(), StateData :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore.

init([]) ->
	{ok, state_name, #state{}}.


%%--------------------------------------------------------------------
%% @doc Set the state name
-spec state_name(
		Event :: term(),
		State :: #state{}
) ->
	{next_state, NextStateName :: atom(), NextState :: #state{}} |
	{next_state, NextStateName :: atom(), NextState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}.

state_name(_Event, State) ->
	{next_state, state_name, State}.


%%--------------------------------------------------------------------
%% @doc Set the state name
-spec state_name(
		Event :: term(),
		From :: {pid(), term()},
		State :: #state{}
) ->
	{next_state, NextStateName :: atom(), NextState :: #state{}} |
	{next_state, NextStateName :: atom(), NextState :: #state{}, timeout() | hibernate} |
	{reply, Reply, NextStateName :: atom(), NextState :: #state{}} |
	{reply, Reply, NextStateName :: atom(), NextState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: normal | term(), NewState :: #state{}} |
	{stop, Reason :: normal | term(), Reply :: term(), NewState :: #state{}}.

state_name(_Event, _From, State) ->
	Reply = ok,
	{reply, Reply, state_name, State}.


%%--------------------------------------------------------------------
%% @doc Handle messages
-spec handle_event(
		Event :: term(),
		StateName :: atom(),
		StateData :: #state{}
) ->
	{next_state, NextStateName :: atom(), NewStateData :: #state{}} |
	{next_state, NextStateName :: atom(), NewStateData :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewStateData :: #state{}}.

handle_event(_Event, StateName, State) ->
	{next_state, StateName, State}.


%%--------------------------------------------------------------------
%% @doc Handle sync messages
-spec handle_sync_event(
		Event :: term(),
		From :: {pid(), Tag :: term()},
		StateName :: atom(),
		StateData :: term()
) ->
	{reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
	{reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
	{next_state, NextStateName :: atom(), NewStateData :: term()} |
	{next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
	{stop, Reason :: term(), NewStateData :: term()}.

handle_sync_event(_Event, _From, StateName, State) ->
	Reply = ok,
	{reply, Reply, StateName, State}.


%%--------------------------------------------------------------------
%% @doc Handle other messages
-spec handle_info(
		Info :: term(),
		StateName :: atom(),
		StateData :: term()
) ->
	{next_state, NextStateName :: atom(), NewStateData :: term()} |
	{next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
	{stop, Reason :: normal | term(), NewStateData :: term()}.

handle_info(_Info, StateName, State) ->
	{next_state, StateName, State}.


%%--------------------------------------------------------------------
%% @doc Terminate gen_fsm
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
	{ok, NextStateName :: atom(), NewStateData :: #state{}}.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.
