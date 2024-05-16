%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2023, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 01. Dec 2023 20:36
%%%-------------------------------------------------------------------
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

-ifndef(A_RECORDS_LOGGER).
-define(A_RECORDS_LOGGER, 1).

-record(a_logger_file_state,{

	path = "NoDefinedPath" :: string(),
	io_device :: file:io_device(),
	separator = " " :: string(),
	message_time = true :: boolean(),
	message_closing = true :: boolean(),
	message_closing_text = "NoDefinedMessageClosing" :: string(),
	message_closing_type = "NoDefinedMessageType" :: string(),
	message_opening = true :: boolean(),
	message_opening_text = "NoDefinedMessageOpening" :: string(),
	message_opening_type = "NoDefinedMessageOpeningType" :: string(),
	message_type = false :: boolean(),
	error_message = true :: boolean(),
	error_message_time = true :: boolean(),
	error_message_type = "NoDefinedErrorMessageType" :: string(),
	error_callback = false :: boolean(),
	error_callback_module = erlang :: module(),
	error_callback_function = display :: atom,
	file_path = "NoDefinedFilePath" :: string(),
	file_name = "log" :: string(),
	file_extension = "txt":: string(),
	file_start_time = true :: boolean(),
	file_closing_time = true :: boolean(),
	init_by_call = true :: boolean(),
	on_init = false :: boolean(),
	on_init_module = undefined :: atom(),
	on_init_function = undefined :: atom(),
	on_init_parameters = [] :: list()
}).

-endif. %% A_RECORDS_LOGGER