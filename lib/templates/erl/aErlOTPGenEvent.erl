%%% -------------------------------------------------------------------
%%% @doc
%%% @notice
%%%
%%% @copyright Arboreus (http://arboreus.systems)
%%% @author Alexandr Kirilov (http://alexandr.kirilov.me)
%%% @created 01/25/2019 at 15:59
%%% -------------------------------------------------------------------
-module(aErlOTPGenEvent).
-author("Alexandr Kirilov (http://alexandr.kirilov.me)").
-behaviour(gen_event).

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
	add_handler/0,
	init/1,
	terminate/2,
	code_change/3,
	
	%% Gen_event functionality
	handle_event/2,
	handle_call/2,
	handle_info/2

]).


%% -------------------------------------------------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (aErlOTPGenEvent) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (aErlOTPGenEvent) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% -------------------------------------------------------------------
%% @doc Start gen_fsm process
-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}}.

start_link() -> gen_event:start_link({local, ?SERVER}).


%% -------------------------------------------------------------------
%% @doc Add handler for event
-spec(add_handler() -> ok | {'EXIT', Reason :: term()} | term()).

add_handler() -> gen_event:add_handler(?SERVER, ?MODULE, []).


%%--------------------------------------------------------------------
%% @doc Initiate Gen_event process
-spec init(InitArgs :: term()) ->
	{ok, State :: #state{}} |
	{ok, State :: #state{}, hibernate} |
	{error, Reason :: term()}.

init([]) -> {ok, #state{}}.


%%--------------------------------------------------------------------
%% @doc Set event handler
-spec handle_event(Event :: term(), State :: #state{}) ->
	{ok, NewState :: #state{}} |
	{ok, NewState :: #state{}, hibernate} |
	{
		swap_handler,
		Args1 :: term(),
		NewState :: #state{},
		Handler2 :: (atom() | {atom(), Id :: term()}),
		Args2 :: term()
	} |
	remove_handler.

handle_event(_Event, State) -> {ok, State}.


%%--------------------------------------------------------------------
%% @doc Set call handler
-spec handle_call(Request :: term(), State :: #state{}) ->
	{ok, Reply :: term(), NewState :: #state{}} |
	{ok, Reply :: term(), NewState :: #state{}, hibernate} |
	{
		swap_handler,
		Reply :: term(),
		Args1 :: term(),
		NewState :: #state{},
		Handler2 :: (atom() | {atom(), Id :: term()}),
		Args2 :: term()
	} |
	{remove_handler, Reply :: term()}.

handle_call(_Request, State) ->
	Reply = ok,
	{ok, Reply, State}.


%%--------------------------------------------------------------------
%% @doc Set other messages handler 
-spec handle_info(Info :: term(), State :: #state{}) ->
	{ok, NewState :: #state{}} |
	{ok, NewState :: #state{}, hibernate} |
	{
		swap_handler,
		Args1 :: term(),
		NewState :: #state{},
		Handler2 :: (atom() | {atom(), Id :: term()}),
		Args2 :: term()
	} |
	remove_handler.

handle_info(_Info, State) ->
	{ok, State}.

%%--------------------------------------------------------------------
%% @doc Terminate gen_event process 
-spec terminate(
		Args :: (
		term() |
		{stop, Reason :: term()} |
		stop |
		remove_handler |
		{error, {'EXIT', Reason :: term()}} |
		{error, term()}
		),
		State :: term()
) -> term().

terminate(_Arg, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @doc Change code
-spec code_change(
		OldVsn :: term() | {down, term()},
		State :: #state{},
		Extra :: term()
) -> {ok, NewState :: #state{}}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
