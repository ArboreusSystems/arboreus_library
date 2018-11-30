%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc A_log events handler
%%%
%%% @end
%%% Created : 06. Май 2018 15:34
%%%-------------------------------------------------------------------
-module(alog_handler).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").
-behaviour(gen_event).

%% Data types
-include("../../data_models/types/types_general.hrl").
-include("../../data_models/types/types_otp.hrl").

%% Data models

%% Constants

%% API
-export([
	start/0,start/1,start/2,
	start_link/0,start_link/1,start_link/2
]).

-export([
	add_handler/0]).

%% gen_event callbacks
-export([init/1,
	handle_event/2,
	handle_call/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER,?MODULE).

-record(state,{}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @end
%%--------------------------------------------------------------------
-spec(add_handler() -> ok | {'EXIT', Reason :: term()} | term()).
add_handler() ->
	gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(InitArgs :: term()) ->
	{ok, State :: #state{}} |
	{ok, State :: #state{}, hibernate} |
	{error, Reason :: term()}).
init([]) ->
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), State :: #state{}) ->
	{ok, NewState :: #state{}} |
	{ok, NewState :: #state{}, hibernate} |
	{swap_handler, Args1 :: term(), NewState :: #state{},
		Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
	remove_handler).
handle_event(_Event, State) ->
	{ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), State :: #state{}) ->
	{ok, Reply :: term(), NewState :: #state{}} |
	{ok, Reply :: term(), NewState :: #state{}, hibernate} |
	{swap_handler, Reply :: term(), Args1 :: term(), NewState :: #state{},
		Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
	{remove_handler, Reply :: term()}).
handle_call(_Request, State) ->
	Reply = ok,
	{ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), State :: #state{}) ->
	{ok, NewState :: #state{}} |
	{ok, NewState :: #state{}, hibernate} |
	{swap_handler, Args1 :: term(), NewState :: #state{},
		Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
	remove_handler).
handle_info(_Info, State) ->
	{ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Args :: (term() | {stop, Reason :: term()} | stop |
remove_handler | {error, {'EXIT', Reason :: term()}} |
{error, term()}), State :: term()) -> term()).
terminate(_Arg, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
		Extra :: term()) ->
	{ok, NewState :: #state{}}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%% ----------------------------
%% @doc Start event manager process, wrapper for gen_event:start/0
-spec start() -> {ok,pid()} | {error,{already_started, pid()}}.

start() -> gen_event:start().


%% ----------------------------
%% @doc Start event manager process, wrapper for gen_event:start/1
-spec start(Name_scope) -> {ok,pid()} | {error,{already_started, pid()}}
	when
	Name_scope :: name_scope().
	
start(Name_scope) -> gen_event:start(Name_scope).


%% ----------------------------
%% @doc Start event manager process, wrapper for gen_event:start/2
-spec start(Name_scope,Options) -> {ok,pid()} | {error,{already_started, pid()}}
	when
	Name_scope :: name_scope(),
	Options :: list_of_options().

start(Name_scope,Options) -> gen_event:start(Name_scope,Options).


%% ----------------------------
%% @doc Create a linked event manager, wrapper for gen_event:start_link/0
-spec start_link() -> {ok,pid()} | {error,{already_started, pid()}}.

start_link() -> gen_event:start_link().


%% ----------------------------
%% @doc Create a linked event manager, wrapper for gen_event:start_link/1
-spec start_link(Name_scope) -> {ok,pid()} | {error,{already_started, pid()}}
	when
	Name_scope :: name_scope().

start_link(Name_scope) -> gen_event:start_link(Name_scope).


%% ----------------------------
%% @doc Create a linked event manager, wrapper for gen_event:start_link/2
-spec start_link(Name_scope,Options) -> {ok,pid()} | {error,{already_started, pid()}}
	when
	Name_scope :: name_scope(),
	Options :: list_of_options().

start_link(Name_scope,Options) -> gen_event:start_link(Name_scope,Options).