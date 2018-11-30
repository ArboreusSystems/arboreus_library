%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Application log file handler
%%%
%%% @end
%%% Created : 14. Апр. 2018 15:24
%%%-------------------------------------------------------------------
-module(alog_file_handler).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").
-behaviour(gen_server).

%% System include
-include("../../data_models/types/types_general.hrl").

%% Constants
-define(SERVER,?MODULE).
-define(DEFAULT_FILE_LIMIT,3000).
-define(DEFAULT_TIMEOUT,60000).

%% Data models
-record(state,{
	file :: file:io_device(),
	path :: unix_path_string(),
	limit :: integer(),
	timeout :: integer()
}).

%% API
-export([
	
	%% Application functionality
	test/0,
	start/1,start/2,
	stop/0,
	
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

start(default,File_path) ->
	start(?SERVER,File_path);
start(Process_name,File_path) ->
	gen_server:start_link({local,Process_name},?MODULE,[File_path],[]).


%% ----------------------------
%% @doc Stop the server
-spec stop() -> ok.

stop() ->
	gen_server:cast(?MODULE,stop).


%% ----------------------------
%% @doc Initializes the server
-spec init(Arguments) ->
	{ok, State} | {ok, State, timeout() | hibernate} | {stop, Reason} | ignore
	when
	Arguments :: term(),
	State :: #state{},
	Reason :: term().

init([Process_id,File_path]) ->
	Full_path = lists:concat([
		File_path,"/",a_var:to_string(Process_id),"_current.log"
	]),
	Create_file = fun() ->
		{ok,File_id} = file:open(Full_path,[append]),
		ok = file:write(File_id,message(start)),
		{ok,#state{
			file = File_id,
			path = Full_path,
			limit = ?DEFAULT_FILE_LIMIT,
			timeout = ?DEFAULT_TIMEOUT
		}}
	end,
	case filelib:is_regular(Full_path) of
		false -> Create_file();
		_ ->
			{ok,File_old} = file:open(Full_path,[append]),
			ok = file:write(File_old,message(close)),
			ok = file:rename(Full_path,lists:concat([
				File_path,"/",Process_id,"_",
				a_var:to_string(a_time:current(timestamp)),".log"
			])),
			ok = file:close(File_old),
			Create_file()
	end.


%% ----------------------------
%% @doc Handling call messages
-spec handle_call(Request, From, State) ->
	{reply, Reply, NewState} | {reply, Reply, NewState, timeout() | hibernate} |
	{noreply, NewState} | {noreply, NewState, timeout() | hibernate} |
	{stop, Reason, Reply, NewState} | {stop, Reason, NewState}
	when
	Request :: term(),
	From :: {Pid, Tag},
	Pid :: pid(),
	Tag :: term(),
	Reply :: term(),
	State :: #state{},
	NewState :: #state{},
	Reason :: term().

handle_call({write,_Message},_From,State) ->
	{reply,ok,State};
handle_call(_Request,_From,State) ->
	{reply,ok,State}.


%% ----------------------------
%% @doc Handling cast messages
-spec handle_cast(Request, State) ->
	{noreply, NewState} | {noreply, NewState, timeout() | hibernate} |
	{stop, Reason, NewState}
	when
	Request :: term(),
	State :: #state{},
	NewState :: #state{},
	Reason :: term().

handle_cast(stop,State) ->
	{stop,normal,State};
handle_cast(_Request, State) ->
	{noreply, State}.


%% ----------------------------
%% @doc Handling all non call/cast messages
-spec handle_info(Info | term(), State) ->
	{noreply, NewState} | {noreply, NewState, timeout() | hibernate} | {stop, Reason, NewState}
	when
	Info :: timeout(),
	State :: #state{},
	NewState :: #state{},
	Reason :: term().

handle_info(timeout,State) ->
	{noreply,State,State#state.timeout};
handle_info(_Info,State) -> {noreply, State}.


%% ----------------------------
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(Reason, State) -> term()
	when
	Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State :: #state{}.

terminate(_Reason,State) ->
	file:close(State#state.file).


%% ----------------------------
%% @doc Convert process state when code is changed
-spec code_change(OldVsn | {down, term()}, State, Extra) ->
	{ok, NewState} | {error, Reason}
	when
	OldVsn :: term(),
	State :: #state{},
	Extra :: term(),
	NewState :: #state{},
	Reason :: term().

code_change(_OldVsn,State,_Extra) ->
	{ok,State}.


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
	