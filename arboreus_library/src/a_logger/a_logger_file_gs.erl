%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2023, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 01. Dec 2023 20:00
%%%-------------------------------------------------------------------
-module(a_logger_file_gs).
-author("Alexandr Kirilov, https://alexandr.kirilov.me").
-behaviour(gen_server).

%% Data types
-include_lib("../include/records/records_a_logger.hrl").

%% API
-export([
	test/0,
	start_link/1,
	init/1,
	handle_call/3,handle_cast/2,handle_info/2,
	terminate/2,
	code_change/3
]).

%% Definitions
-define(SERVER,?MODULE).

%% Data models



%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%%%===================================================================
%%% API
%%%===================================================================
%% ----------------------------
%% @doc Spawns the server and registers the local name (unique)
-spec start_link(INITIAL_STATE) -> {ok,PID} | ignore | {error,REASON}
	when
		INITIAL_STATE :: #a_logger_file_state{},
		PID :: pid(),
		REASON :: term().

start_link(INITIAL_STATE) when is_record(INITIAL_STATE,a_logger_file_state) ->

	gen_server:start_link(?MODULE,[INITIAL_STATE],[]);

start_link(INITIAL_STATE) ->

	{error,[{"INITIAL_STATE",is_record(INITIAL_STATE,a_logger_file_state)}]}.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% ----------------------------
%% @private
%% @doc Initializes the server
-spec init(ARGUMENTS) ->
	{ok,STATE} | {ok,STATE,TIMEOUT} | {stop,REASON} | {error,REASON} | ignore
	when
		ARGUMENTS :: term(),
		STATE :: #a_logger_file_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

init([INITIAL_STATE]) ->

	case filelib:ensure_path(INITIAL_STATE#a_logger_file_state.path) of
		ok ->
			START_TIME = integer_to_list(a_time_now:microseconds()),
			FILE_PATH =
				INITIAL_STATE#a_logger_file_state.path ++ "/" ++
				INITIAL_STATE#a_logger_file_state.file_name ++ "_" ++ START_TIME ++ "." ++
				INITIAL_STATE#a_logger_file_state.file_extension,
			case file:open(FILE_PATH,[write]) of
				{ok,IO_DEVICE} ->
					if
						INITIAL_STATE#a_logger_file_state.message_opening == true ->
							TIME = case INITIAL_STATE#a_logger_file_state.file_start_time of
								true ->
									integer_to_list(a_time_now:microseconds()) ++
									INITIAL_STATE#a_logger_file_state.separator;
								_ -> ""
							end,
							TYPE = case INITIAL_STATE#a_logger_file_state.message_type of
								true ->
									INITIAL_STATE#a_logger_file_state.message_opening_type ++
									INITIAL_STATE#a_logger_file_state.separator;
								_ -> ""
							end,
							OPENING_MESSAGE =
								TIME ++ TYPE ++
								INITIAL_STATE#a_logger_file_state.message_opening_text ++
								"\n",
							file:write(IO_DEVICE,OPENING_MESSAGE)
					end,
					on_init(INITIAL_STATE),
					process_flag(trap_exit, true),
					{ok,INITIAL_STATE#a_logger_file_state{
						io_device = IO_DEVICE,
						file_path = FILE_PATH
					}};
				REPLY -> REPLY
			end;
		REPLY -> REPLY
	end;

init([]) -> {error,wrong_initial_data}.


%% ----------------------------
%% @private
%% @doc Handling call messages
-spec handle_call(REQUEST,FROM,STATE) ->
	{reply,REPLY,NEW_STATE} | {reply,REPLY,NEW_STATE,TIMEOUT} |	{noreply,NEW_STATE} |
	{noreply,NEW_STATE,TIMEOUT} | {stop,REASON,REPLY,NEW_STATE} | {stop,REASON,NEW_STATE}
	when
		REQUEST :: term(),
		FROM :: {PID,TAG},PID :: pid(),TAG :: term(),
		REPLY :: term(),
		STATE :: #a_logger_file_state{},
		NEW_STATE :: #a_logger_file_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

handle_call(logbook_descriptor,_FROM,STATE = #a_logger_file_state{}) ->

	{reply,{ok,STATE#a_logger_file_state.io_device},STATE};

handle_call({write_binary,MESSAGE_BODY},_FROM,STATE = #a_logger_file_state{}) ->

	{reply,write_binary_message(STATE,MESSAGE_BODY),STATE};

handle_call({write_text,MESSAGE_BODY},_FROM,STATE = #a_logger_file_state{}) ->

	{reply,write_text_message(STATE,MESSAGE_BODY),STATE};

handle_call(REQUEST,FROM,STATE = #a_logger_file_state{}) ->

	ERROR = {error,undefined_call,self(),REQUEST,FROM,?MODULE,?FILE,?LINE},
	write_error(STATE,a_string:from_term(ERROR)),
	a_error:callback(
		STATE#a_logger_file_state.error_callback,
		STATE#a_logger_file_state.error_callback_module,
		STATE#a_logger_file_state.error_callback_function,
		[ERROR]
	),
	{reply,ERROR,STATE}.


%% ----------------------------
%% @private
%% @doc Handling cast messages
-spec handle_cast(REQUEST,STATE) ->
	{noreply,NEW_STATE} | {noreply,NEW_STATE,TIMEOUT} | {stop,REASON,NEW_STATE}
	when
		REQUEST :: term(),
		STATE :: #a_logger_file_state{},
		NEW_STATE :: #a_logger_file_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

handle_cast({write_binary,MESSAGE_BODY},STATE = #a_logger_file_state{}) ->

	write_binary_message(STATE,MESSAGE_BODY),
	{noreply,STATE};

handle_cast({write_text,MESSAGE_BODY},STATE = #a_logger_file_state{}) ->

	write_text_message(STATE,MESSAGE_BODY),
	{noreply,STATE};

handle_cast(REQUEST,STATE = #a_logger_file_state{}) ->

	ERROR = {error,undefined_cast,self(),REQUEST,?MODULE,?FILE,?LINE},
	write_error(STATE,a_string:from_term(ERROR)),
	a_error:callback(
		STATE#a_logger_file_state.error_callback,
		STATE#a_logger_file_state.error_callback_module,
		STATE#a_logger_file_state.error_callback_function,
		[ERROR]
	),
	{noreply,STATE}.


%% ----------------------------
%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(INFO,STATE) ->
	{noreply,NEW_STATE} | {noreply,NEW_STATE,TIMEOUT} | {stop,REASON,NEW_STATE}
	when
		INFO :: timeout() | term(),
		STATE :: #a_logger_file_state{},
		NEW_STATE :: #a_logger_file_state{},
		TIMEOUT :: timeout() | hibernate,
		REASON :: term().

handle_info({'EXIT',_FROM,REASON},STATE) ->

	{stop,REASON,STATE};

handle_info(INFO,STATE = #a_logger_file_state{}) ->

	ERROR = {error,undefined_info,self(),INFO,?MODULE,?FILE,?LINE},
	write_error(STATE,a_string:from_term(ERROR)),
	a_error:callback(
		STATE#a_logger_file_state.error_callback,
		STATE#a_logger_file_state.error_callback_module,
		STATE#a_logger_file_state.error_callback_function,
		[ERROR]
	),
	{noreply,STATE}.


%% ----------------------------
%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(REASON,STATE) -> term()
	when
		REASON :: normal | shutdown | {shutdown,term()} | term(),
		STATE :: #a_logger_file_state{}.

terminate(_REASON,STATE = #a_logger_file_state{}) ->

	case STATE#a_logger_file_state.io_device of
		undefined -> {error,undefined};
		_ ->
			if
				STATE#a_logger_file_state.message_closing == true ->
					TIME = case STATE#a_logger_file_state.file_closing_time of
						true ->
							integer_to_list(a_time_now:microseconds()) ++
							STATE#a_logger_file_state.separator;
						_ -> ""
					end,
					TYPE = case STATE#a_logger_file_state.message_type of
						true ->
							STATE#a_logger_file_state.message_closing_type ++
							STATE#a_logger_file_state.separator;
						_ -> ""
					end,
					file:write(
						STATE#a_logger_file_state.io_device,
						TIME ++ TYPE ++ STATE#a_logger_file_state.message_closing_text ++ "\n"
					)
			end,
			case file:close(STATE#a_logger_file_state.io_device) of
				ok -> ok;
				{error,REASON} -> {error,REASON}
			end
	end.


%% ----------------------------
%% @private
%% @doc Convert process state when code is changed
-spec code_change(OLD_VERSION,STATE,EXTRA) -> {ok,NEW_STATE} | {error,REASON}
	when
		OLD_VERSION :: term() | {down,term()},
		STATE :: #a_logger_file_state{},
		NEW_STATE :: #a_logger_file_state{},
		EXTRA :: term(),
		REASON :: term().

code_change(_OLD_VERSION,STATE = #a_logger_file_state{},_EXTRA) ->

	{ok,STATE}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% ----------------------------
%% @doc Perform on-init function
-spec on_init(STATE) -> ok
	when STATE :: #a_logger_file_state{}.

on_init(STATE) when STATE#a_logger_file_state.on_init == true ->

	erlang:apply(
		STATE#a_logger_file_state.on_init_module,
		STATE#a_logger_file_state.on_init_function,
		STATE#a_logger_file_state.on_init_parameters
	);

on_init(_STATE) -> ok.


%% ----------------------------
%% @doc Write error message to logbook file
-spec write_error(STATE,MESSAGE_BODY) -> ok | {error, REASON}
	when
		STATE :: #a_logger_file_state{},
		MESSAGE_BODY :: string(),
		REASON :: term().

write_error(STATE,MESSAGE_BODY) ->

	if
		STATE#a_logger_file_state.error_message == true ->
			TIME = case STATE#a_logger_file_state.error_message_time of
				true ->
					integer_to_list(a_time_now:microseconds()) ++
					STATE#a_logger_file_state.separator;
				_ -> ""
			end,
			TYPE = case STATE#a_logger_file_state.message_type of
				true ->
					STATE#a_logger_file_state.error_message_type ++
					STATE#a_logger_file_state.separator;
				_ -> ""
			end,
			case STATE#a_logger_file_state.io_device of
				undefined -> {error,no_io_device};
				IO_DEVICE -> file:write(IO_DEVICE,TIME ++ TYPE ++ MESSAGE_BODY)
			end
	end.


%% ----------------------------
%% @doc Write message to logbook file
-spec write_text_message(STATE,MESSAGE_BODY) -> ok | {error, REASON}
	when
		STATE :: #a_logger_file_state{},
		MESSAGE_BODY :: string(),
		REASON :: term().

write_text_message(STATE,MESSAGE_BODY) ->

	TIME = case STATE#a_logger_file_state.message_time of
		true ->
			integer_to_list(a_time_now:microseconds()) ++
			STATE#a_logger_file_state.separator;
		_ -> ""
	end,
	case STATE#a_logger_file_state.io_device of
		undefined -> {error,no_io_device};
		IO_DEVICE -> file:write(IO_DEVICE,TIME ++ MESSAGE_BODY)
	end.


%% ----------------------------
%% @doc Write message to logbook file
-spec write_binary_message(STATE,MESSAGE_BODY) -> ok | {error, REASON}
	when
		STATE :: #a_logger_file_state{},
		MESSAGE_BODY :: binary(),
		REASON :: term().

write_binary_message(STATE,MESSAGE_BODY) ->

	TIME = case STATE#a_logger_file_state.message_time of
		true ->
			integer_to_list(a_time_now:microseconds()) ++
			STATE#a_logger_file_state.separator;
		_ -> ""
	end,
	case STATE#a_logger_file_state.io_device of
		undefined -> {error,no_io_device};
		IO_DEVICE -> file:write(IO_DEVICE,<<(list_to_binary(TIME))/binary,MESSAGE_BODY/binary>>)
	end.