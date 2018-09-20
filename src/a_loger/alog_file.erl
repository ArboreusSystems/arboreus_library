%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Log file handler
%%%
%%% @end
%%% Created : 14. Апр. 2018 20:53
%%%-------------------------------------------------------------------
-module(alog_file).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").
-behaviour(gen_server).

%% System include
-include("../data_models/types/types_general.hrl").

%% Constants
-define(SERVER,?MODULE).
-define(DEFAULT_FILE_SIZE_LIMIT,1).
-define(DEFAULT_TIMEOUT,10000).

%% Data models
-record(state,{
	file :: file:io_device(),
	process_name :: any(),
	dir :: unix_path_string(),
	current_file :: unix_path_string(),
	limit :: integer(),
	timeout :: integer()
}).

%% API
-export([
	
	%% Application functionality
	test/0,
	start/1,start/2,
	stop/0,stop/1,
	set_timeout/1,
	set_limit/1,
	
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

test() -> ok.


%% ----------------------------
%% @doc Setting file size limit
-spec set_limit(Kind) -> ok
	when
	Kind :: {for_default,Limit} | {for_process,Process_id,Limit},
	Limit :: pos_integer(),
	Process_id :: {global,Process} | atom(),
	Process :: term() | atom() | any().

set_limit({for_default,Limit}) ->
	set_limit({for_process,?SERVER,Limit});
set_limit({for_process,Process_id,Limit}) ->
	gen_server:call(Process_id,{set_limit,Limit}).


%% ----------------------------
%% @doc Setting up timeout
-spec set_timeout(Kind) -> ok
	when
	Kind :: {for_default,Timeout} | {for_process,Process_id,Timeout},
	Timeout :: pos_integer(),
	Process_id :: {global,Process} | atom(),
	Process :: term() | atom() | any().

set_timeout({for_default,Timeout}) ->
	set_timeout({for_process,?SERVER,Timeout});
set_timeout({for_process,Process_id,Timeout}) ->
	ok = gen_server:call(Process_id,timeout_stop),
	ok = gen_server:call(Process_id,{set_timeout,Timeout}),
	gen_server:call(Process_id,timeout_start).


%% ----------------------------
%% @doc Default start the server
-spec start(File_path) -> {ok,Pid} | ignore | {error,Reason}
	when
	File_path :: unix_path_string(),
	Pid :: pid(),
	Reason :: term().

start(File_path) -> start(default,File_path).


%% ----------------------------
%% @doc Start the server
-spec start(Process_name,File_path) -> {ok,Pid} | ignore | {error,Reason}
	when
	Process_name :: default | atom(),
	File_path :: unix_path_string(),
	Pid :: pid(),
	Reason :: term().

start(default,Dir) ->
	start(?SERVER,Dir);
start(Process_name,Dir) ->
	{ok,Pid} = gen_server:start_link(
		{local,Process_name},?MODULE,[Process_name,Dir],[
			{debug,[
				trace,log,
				{log_to_file,"/Users/alexandr/projects/Arboreus/Arboreus_fw_library/log/mhcl_user_trace.log"}]}
	]),
	ok = gen_server:call(Pid,timeout_start),
	{ok,Pid}.


%% ----------------------------
%% @doc Default stop the server
-spec stop() -> ok.

stop() -> stop(?SERVER).


%% ----------------------------
%% @doc Stop the process by name
-spec stop(Process) -> ok
	when
	Process :: atom() | pid().

stop(Process) -> gen_server:cast(Process,stop).


%% ----------------------------
%% @doc Initializes the server
-spec init(Arguments) ->
	{ok,State} | {ok,State,Time_type} | {stop,Reason} | ignore
	when
	Time_type :: timeout() | hibernate,
	Arguments :: term(),
	State :: #state{},
	Reason :: term().

init([Process_name,Dir]) ->
	Current_file_path = full_path(current_file,Dir,Process_name),
	{ok,#state{
		file = case filelib:is_regular(Current_file_path) of
			false ->
				{ok,File_id} = create_log_file(Current_file_path),
				File_id;
			_ ->
				Closed_file_path = full_path(closed_file,Dir,Process_name),
				{ok,File} = file:open(Current_file_path,[append]),
				{ok,File_id} = refresh_log_file(File,Current_file_path,Closed_file_path),
				File_id
		end,
		process_name = Process_name,
		dir = Dir,
		current_file = Current_file_path,
		limit = ?DEFAULT_FILE_SIZE_LIMIT,
		timeout = ?DEFAULT_TIMEOUT
	}}.


%% ----------------------------
%% @doc Handling call messages
-spec handle_call(Request,From,State) ->
	{reply,Reply,NewState} | {reply,Reply,NewState,Time_type} |
	{noreply,NewState} | {noreply,NewState,Time_type} |
	{stop,Reason,Reply, NewState} | {stop,Reason,NewState}
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

handle_call({set_limit,Limit},_From,State) ->
	{reple,ok,State#state{limit = Limit}};
handle_call({set_timeout,Timeout},_From,State) ->
	{reply,ok,State#state{timeout = Timeout}};
handle_call(timeout_stop,_From,State) ->
	{reply,ok,State};
handle_call(timeout_start,_From,State) ->
	{reply,ok,State,State#state.timeout};
handle_call(_Request,_From,State) ->
	{reply,ok,State}.


%% ----------------------------
%% @doc Handling cast messages
-spec handle_cast(Request,State) ->
	{noreply,NewState} | {noreply,NewState,Time_type} |
	{stop,Reason,NewState}
	when
	Time_type :: timeout() | hibernate,
	Request :: term(),
	State :: #state{},
	NewState :: #state{},
	Reason :: term().

handle_cast(stop,State) -> {stop,normal,State};
handle_cast(_Request,State) -> {noreply,State}.


%% ----------------------------
%% @doc Handling all non call/cast messages
-spec handle_info(Info,State) ->
	{noreply,NewState} | {noreply,NewState,Time_type} | {stop,Reason,NewState}
	when
	Time_type :: timeout() | hibernate,
	Info :: timeout() | term(),
	State :: #state{},
	NewState :: #state{},
	Reason :: term().

handle_info(timeout,State) ->
	case timeout_handler(State) of
		{ok,File} ->
			{noreply,State#state{file = File},State#state.timeout};
		_ ->
			{noreply,State,State#state.timeout}
	end;
handle_info(_Info,State) -> {noreply,State}.


%% ----------------------------
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(Reason,State) -> term()
	when
	Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State :: #state{}.


terminate(_Reason,State) ->
	file:close(State#state.file).


%% ----------------------------
%% @doc Convert process state when code is changed
-spec code_change(OldVsn,State,Extra) ->
	{ok,NewState} | {error,Reason}
	when
	OldVsn :: term() | {down, term()},
	State :: #state{},
	Extra :: term(),
	NewState :: #state{},
	Reason :: term().

code_change(_OldVsn,State,_Extra) ->
	{ok, State}.


%% ----------------------------
%% @doc Default timeout handler
-spec timeout_handler(State) -> ok
	when
	State :: state().

timeout_handler(State) ->
	File_size = filelib:file_size(State#state.current_file),
	if
		File_size > State#state.limit ->
			{ok,New_current_file} = refresh_log_file(
				State#state.file,
				State#state.current_file,
				full_path(closed_file,State#state.dir,State#state.process_name)
			),
			io:format("Log file refreshing: ~p~n",[{ok,New_current_file}]);
		true -> ok
	end.


%% ----------------------------
%% @doc Create current log file
-spec create_log_file(Current_file_path) -> {ok,File_id}
	when
	Current_file_path :: unix_path_string(),
	File_id :: file:io_device().

create_log_file(Current_file_path) ->
	{ok,File_id} = file:open(Current_file_path,[append]),
	file:write(File_id,message(start)),
	{ok,File_id}.


%% ----------------------------
%% @doc Refresh current log file
-spec refresh_log_file(File_old_id,Current_file_path,Closed_file_path) -> {ok,File_new_id}
	when
	File_old_id :: file:io_device(),
	File_new_id :: file:io_device(),
	Current_file_path :: unix_path_string(),
	Closed_file_path :: unix_path_string().

refresh_log_file(File,Current_file_path,Closed_file_path) ->
	io:format("File: ~p~n",[File]),
	Write = file:write(File,message(close)),
	io:format("Write: ~p~n",[Write]),
	ok = file:close(File),
	ok = file:rename(Current_file_path,Closed_file_path),
	create_log_file(Current_file_path).


%% ----------------------------
%% @doc Return full path for log file
-spec full_path(Kind,Dir,Process_name) -> unix_path_string()
	when
	Kind :: current_file | closed_file,
	Dir :: unix_path_string(),
	Process_name :: atom().
	

full_path(current_file,Dir,Process_name) ->
	lists:concat([
		Dir,"/",a_var:to_string(Process_name),"_current.log"
	]);
full_path(closed_file,Dir,Process_name) ->
	lists:concat([
		Dir,"/",Process_name,"_",
		a_var:to_string(a_time:current(timestamp)),".log"
	]).


%% ----------------------------
%% @doc Create message handler messages for appending it to file
-spec message(Kind) -> utf_text_string()
	when
	Kind :: close | start.

message(close) ->
	lists:concat([
		"\n\nFile closed at ",
		a_var:to_string(a_time:format(rfc850,{date_tuple,erlang:localtime()})),"\n",
		"Timestamp: ",a_var:to_string(a_time:current(timestamp))
	]);
message(start) ->
	lists:concat([
		"File started at ",
		a_var:to_string(a_time:format(rfc850,{date_tuple,erlang:localtime()})),"\n",
		"Timestamp: ",a_var:to_string(a_time:current(timestamp)),"\n\n"
	]).