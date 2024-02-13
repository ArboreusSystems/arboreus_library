%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus users: user authorisation server
%%%
%%% @end
%%% Created : 06/03/2018 at 13:22
%%%-------------------------------------------------------------------
-module(ause_auth).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").
-behaviour(gen_server).

%% Constants
-define(SERVER, ?MODULE).

%% Data types
-include("../include/types/types_a_general.hrl").
-include("../include/types/types_a_time.hrl").
-include("../include/types/types_a_users.hrl").

%% Data models
-include("../include/records/records_a_users.hrl").
-record(state, {}).

%% API
-export([
	
	%% Module functionality
	test/0, test/1,
	start_link/0, start_link/1,
	start/0, start/1,
	stop/0, stop/1,
	
	%% Application functionality
	verify_password/2,
	
	%% Gen_server functionality
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3

]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> test(trace).


%% ----------------------------
%% @doc Module test function
-spec test(Kind) -> ok
	when
	Kind :: trace | normal | pid().

test(trace) ->
	{ok, Pid} = start(),
	sys:trace(Pid, true),
	test(Pid);
test(normal) ->
	{ok, Pid} = start(),
	test(Pid);
test(Pid) when is_pid(Pid) ->
	Time_start =  a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format("Process ~p started at: ~p (~p)~n", [
		Pid,a_time:from_timestamp(rfc850,Time_start),Time_start
	]),
	io:format("Ok. Process name ~p.~n", [?SERVER]),
	A_user_password = "test_password",
	A_user_password_hash = a_user:generate_password_hash(A_user_password),
	A_user = #a_user{password = A_user_password_hash},
	{ok,A_user_id} = a_user:create(A_user),
	io:format("DONE! Test user created: ~p~n",[A_user_id]),
	{ok,A_user_id} = verify_password(A_user_id,A_user_password),
	{norow,1} = verify_password(1,A_user_password),
	{wrong_password,A_user_id} = verify_password(A_user_id,"1"),
	io:format("DONE! Test user password verification passed.~n"),
	{ok,A_user_id} = a_user:delete(A_user_id),
	{norow,A_user_id} = a_user:read(A_user_id),
	io:format("DONE! Test user deleted: ~p~n",[A_user_id]),
	Time_stop =  a_time:current(timestamp),
	io:format("*** -------------------~nTest for ~p passed~n", [?SERVER]),
	io:format("Finished at: ~p (~p)~n", [
		a_time:from_timestamp(rfc850,Time_stop),Time_stop
	]),
	io:format("Test time is: ~p~n",[Time_stop - Time_start]),
	stop().


%% ----------------------------
%% @doc Module api calls

verify_password(User,Password) ->
	gen_server:call(?SERVER,{verify_password,User,Password}).


%% ----------------------------
%% @doc Handling call messages
-spec handle_call(Request, From, State) ->
	{reply, Reply, NewState} | {reply, Reply, NewState, Time_type} |
	{noreply, NewState} | {noreply, NewState, Time_type} |
	{stop, Reason, Reply, NewState} | {stop, Reason, NewState}
	when
	Time_type :: timeout() | hibernate,
	Request :: term(),
	From :: {Pid, Tag},
	Pid :: pid(),
	Tag :: term(),
	Reply :: term(),
	State :: #state{},
	NewState :: #state{},
	Reason :: term().

handle_call({verify_password,User,Password},_From,State) ->
	{reply,a_user:verify_password(User,Password),State};
handle_call(_Request, _From, State) -> {reply, ok, State}.


%% ----------------------------
%% @doc Handling cast messages
-spec handle_cast(Request, State) ->
	{noreply, NewState} | {noreply, NewState, Time_type} |
	{stop, Reason, NewState}
	when
	Time_type :: timeout() | hibernate,
	Request :: term(),
	State :: #state{},
	NewState :: #state{},
	Reason :: term().

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Request, State) -> {noreply, State}.


%% ----------------------------
%% @doc Handling all non call/cast messages
-spec handle_info(Info, State) ->
	{noreply, NewState} | {noreply, NewState, Time_type} | {stop, Reason, NewState}
	when
	Time_type :: timeout() | hibernate,
	Info :: timeout() | term(),
	State :: #state{},
	NewState :: #state{},
	Reason :: term().

handle_info(_Info, State) -> {noreply, State}.


%% ----------------------------
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(Reason, State) -> term()
	when
	Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State :: #state{}.

terminate(_Reason, _State) -> ok.


%% ----------------------------
%% @doc Convert process state when code is changed
-spec code_change(OldVsn, State, Extra) ->
	{ok, NewState} | {error, Reason}
	when
	OldVsn :: term() | {down, term()},
	State :: #state{},
	Extra :: term(),
	NewState :: #state{},
	Reason :: term().

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% ----------------------------
%% @doc Default start link to the server
-spec start() -> {ok, Pid} | ignore | {error, Reason}
	when
	Pid :: pid(),
	Reason :: term().

start() -> start(default).


%% ----------------------------
%% @doc Start link to the server
-spec start(Server_name) -> {ok, Pid} | ignore | {error, Reason}
	when
	Server_name :: default | atom(),
	Pid :: pid(),
	Reason :: term().

start(default) -> start_link({local, ?SERVER});
start(Server_name) ->
	gen_server:start(Server_name, ?MODULE, [], []).


%% ----------------------------
%% @doc Default start link to the server
-spec start_link() -> {ok, Pid} | ignore | {error, Reason}
	when
	Pid :: pid(),
	Reason :: term().

start_link() -> start_link(default).


%% ----------------------------
%% @doc Start link to the server
-spec start_link(Process_name) -> {ok, Pid} | ignore | {error, Reason}
	when
	Process_name :: default | atom(),
	Pid :: pid(),
	Reason :: term().

start_link(default) -> start_link({local, ?SERVER});
start_link(Server_name) ->
	gen_server:start_link(Server_name, ?MODULE, [], []).


%% ----------------------------
%% @doc Default stop the server
-spec stop() -> ok.

stop() -> stop(?SERVER).


%% ----------------------------
%% @doc Stop the process by name
-spec stop(Process_name :: atom()) -> ok.

stop(Process_name) ->
	gen_server:cast(Process_name, stop).


%% ----------------------------
%% @doc Initializes the server
-spec init(Arguments) ->
	{ok, State} | {ok, State, Time_type} | {stop, Reason} | ignore
	when
	Time_type :: timeout() | hibernate,
	Arguments :: term(),
	State :: #state{},
	Reason :: term().

init([]) -> {ok, #state{}}.