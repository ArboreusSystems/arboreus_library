%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2016, http://arboreus.system
%%% @doc Additional Remote Procedure Call services
%%%
%%% @end
%%% Created : 07. Окт. 2016 19:38
%%%-------------------------------------------------------------------
-module(a_rpc).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% API
-export([
	test/0,
	async_call/4,
	call/4,
	nodes/0,
	hostname/0
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Make asynchronous call on defined node
-spec call(Node_name,Module,Function,Arguments) -> any()
	when
	Node_name :: atom(),
	Module :: atom(),
	Function :: atom(),
	Arguments :: list().

call(Node_name,Module,Function,Arguments) ->
	rpc:call(Node_name,Module,Function,Arguments).


%% ----------------------------
%% @doc Make asynchronous call on defined node
-spec async_call(Node_name,Module,Function,Arguments) -> any()
	when
	Node_name :: atom(),
	Module :: atom(),
	Function :: atom(),
	Arguments :: list().

async_call(Node_name,Module,Function,Arguments) ->
	rpc:yield(rpc:async_call(Node_name,Module,Function,Arguments)).


%% ----------------------------
%% @doc Return list of started nodes on current server
-spec nodes() -> proplists:proplist().

nodes() ->

	HOST_NAME = hostname(),
	[_EPMD | NODE_DESCRIPTIONS] = string:tokens(os:cmd("epmd -names"),"\n"),
	NODES = [parse_node_description(DESCRIPTION) || DESCRIPTION <- NODE_DESCRIPTIONS],
	[{list_to_atom(string:concat(string:concat(NODE_NAME,"@"),HOST_NAME)),PORT} || {NODE_NAME,PORT} <- NODES].


%% ----------------------------
%% @doc Check the node description and return term within node data
-spec parse_node_description(NODE_DESCRIPTION) -> {nomatch,REPLY} | {NODE_NAME_STRING,PORT_NUMBER}
	when
		NODE_DESCRIPTION :: string(),
		REPLY :: any(),
		NODE_NAME_STRING :: string(),
		PORT_NUMBER :: port().

parse_node_description(NODE_DESCRIPTION) ->

	PATTERN = "^name\ ([a-zA-Z_0-9\-]*)\ at port ([0-9]{1,5})$",
	case re:run(NODE_DESCRIPTION,PATTERN,[dotall, ungreedy, {capture, all_but_first, list}]) of
		{match,[NODE_NAME,PORT]} -> {NODE_NAME,list_to_integer(PORT)};
		REPLY -> {nomatch,REPLY}
	end.


%% ----------------------------
%% @doc Return full hostname of current host
-spec hostname() -> string().

hostname() ->

	CMD_OUTPUT = os:cmd("hostname -f"),
	{HOSTNAME,_} = lists:split(length(CMD_OUTPUT) - 1, CMD_OUTPUT),
	HOSTNAME.
