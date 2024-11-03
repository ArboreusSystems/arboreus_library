%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Erlang nodes handler extension
%%%
%%% @end
%%% Created : 07. Март 2018 18:48
%%%-------------------------------------------------------------------
-module(a_node).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Data types
-include("a_includes.hrl").

%% API
-export([

	test/0,

	change_name/1,change_name/2,
	name_type/0,name_type/1,
	parse_name/0,parse_name/1,
	load/1,
	names/0,names/1,
	fqdn/0,
	name_string/2,
	name_atom/2,
	node_id/1,
	default_node_properties/0,
	start/1,
	stop/1,stop/4,
	ensure/4,
	is_started/1,is_started/2,
	is_connected/1,
	connect/1

]).



%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->

	TIME_START = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_node) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850,TIME_START),TIME_START]
	),

	NODE_PROPERTIES = default_node_properties(),

	erlang:display(
		start(NODE_PROPERTIES#a_node_start_properties{
			name = "test_node",
			cookie = "12345"
		})
	),

%%	LOAD_LIST_STRUCTURE = [(fun is_integer/1),(fun is_integer/1),(fun is_integer/1)],
%%	LOAD_LIST = load(list),
%%	LOAD_LIST_WRONG = [wrong,list,structure],
%%	true = a_structure_l:verify(return_boolean,LOAD_LIST_STRUCTURE,LOAD_LIST),
%%	false = a_structure_l:verify(return_boolean,LOAD_LIST_STRUCTURE,LOAD_LIST_WRONG),
%%	LOAD_RECORD_STRUCTURE = {
%%		(fun is_atom/1),
%%		(fun is_integer/1),
%%		(fun is_integer/1),
%%		(fun is_integer/1)
%%	},
%%	LOAD_RECORD = load(record),
%%	LOAD_RECORD_WRONG = #a_node_load{
%%		processes = processes,
%%		memory_total = memory_total,
%%		ports = ports
%%	},
%%	true = a_structure_r:verify(return_boolean,LOAD_RECORD_STRUCTURE,LOAD_RECORD),
%%	false = a_structure_r:verify(return_boolean,LOAD_RECORD_STRUCTURE,LOAD_RECORD_WRONG),
%%	io:format("DONE! Fun load/1 test passed~n"),
%%	NEW_NAME_STRING = "new_name",
%%	CHANGE_NAME_TEST = case node() of
%%		'nonode@nohost' ->
%%			nonetkernel = change_name(test),
%%			OLD_NAME_STRING = "old_name",
%%			{ok,_} = net_kernel:start([list_to_atom(OLD_NAME_STRING)]),
%%			{ok,{OLD_NAME_STRING,DOMAIN}} = parse_name(),
%%			OLD_NAME = node(),
%%			{ok,NEW_NAME} = change_name(list_to_atom(NEW_NAME_STRING)),
%%			{ok,{NEW_NAME_STRING,DOMAIN}} = parse_name(NEW_NAME),
%%			{ok,OLD_NAME} = change_name(list_to_atom(OLD_NAME_STRING)),
%%			net_kernel:stop();
%%		OLD_NODE_NAME ->
%%			{ok,{OLD_NAME_STRING,DOMAIN}} = parse_name(),
%%			case change_name(list_to_atom(NEW_NAME_STRING)) of
%%				{ok,NEW_NAME} ->
%%					{ok,{NEW_NAME_STRING,DOMAIN}} = parse_name(NEW_NAME),
%%					{ok,OLD_NODE_NAME} = change_name(list_to_atom(OLD_NAME_STRING)),
%%					ok;
%%				RESULT -> RESULT
%%			end
%%	end,
%%	io:format("DONE! Change name functionality test result: ~p~n",[CHANGE_NAME_TEST]),

	TIME_STOP = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_node) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850,TIME_STOP),TIME_STOP]
	),
	io:format("Test time is: ~p~n", [TIME_STOP - TIME_START]),
	ok.


%% ----------------------------
%% @doc Node load
-spec load(KIND) -> a_list_of_properties() | #a_node_load{}
	when KIND :: list | record.

load(list)->

	[
		length(erlang:processes()),
		length(erlang:ports()),
		proplists:get_value(total,erlang:memory())
	];

load(record) ->

	#a_node_load{
		processes = length(erlang:processes()),
		ports = length(erlang:ports()),
		memory_total = proplists:get_value(total,erlang:memory())
	}.


%% ----------------------------
%% @doc Parse current node name
-spec parse_name() -> {ok,{NAME,DOMAIN}} | nonetkernel
	when
		NAME :: unicode:charlist(),
		DOMAIN :: unicode:charlist().

parse_name() -> parse_name(node()).


%% ----------------------------
%% @doc Parse node name
-spec parse_name(NODE_NAME) -> {ok,{NAME,DOMAIN}} | {ok,NAME} | nomatch | nonetkernel
	when
		NODE_NAME :: node() | unicode:charlist(),
		NAME :: unicode:charlist(),
		DOMAIN :: unicode:charlist().

parse_name('nonode@nohost') -> nonetkernel;

parse_name(NODE_NAME) when is_atom(NODE_NAME) ->

	parse_name(atom_to_list(NODE_NAME));

parse_name(NODE_NAME) when is_list(NODE_NAME) ->

	case re:run(NODE_NAME,"^([a-zA-Z0-9\_\-]{1,})\@([a-zA-Z0-9\_\-]{1,}\.{0,1}){1,}$") of
		{match,_} ->
			[NAME,DOMAIN] = string:tokens(NODE_NAME,"@"),
			{ok,{NAME,DOMAIN}};
		_ ->
			case re:run(NODE_NAME,"^([a-zA-Z0-9\-\_]{1,})$") of
				{match,_} -> {ok,NODE_NAME};
				_ -> nomatch
			end
	end.


%% ----------------------------
%% @doc Get current node name type
-spec name_type() -> nonetkernel | shortnames | longnames.

name_type() -> name_type(node()).


%% ----------------------------
%% @doc Return node name type
-spec name_type(NAME) -> shortnames | longnames | name | nomatch | nonetkernel
	when NAME :: atom() | unicode:charlist().

name_type('nonode@nohost') -> nonetkernel;

name_type(NAME) when is_atom(NAME) ->

	name_type(atom_to_list(NAME));

name_type(NAME) when is_list(NAME) ->

	case re:run(NAME,"^[a-zA-Z0-9\_\-]{1,}\@[a-zA-Z0-9\_\-]{1,}$") of
		{match,_} -> shortnames;
		_ ->
			case re:run(NAME,"^[a-zA-Z0-9\_\-]{1,}\@[a-zA-Z0-9\.\_\-]{1,}$") of
				{match,_} -> longnames;
				_ ->
					case re:run(NAME,"^[a-zA-Z0-9\_\-]{1,}$") of
						{match,_} -> name;
						_ -> nomatch
					end
			end
	end.


%% ----------------------------
%% @doc Change node name
-spec change_name(NEW_NAME) -> {ok,NEW_NODE_NAME} | wrongnewname | wrongtype | nonetkernel
	when
		NEW_NAME :: atom() | unicode:charlist(),
		NEW_NODE_NAME :: node().

change_name(NEW_NAME) -> change_name(NEW_NAME,name_type()).


%% ----------------------------
%% @doc Change node name
-spec change_name(NEW_NAME,TYPE) ->
	{ok,NEW_NODE_NAME} | wrongnewname | wrongtype | nonetkernel | NET_KERNEL_ERROR
	when
		NEW_NAME :: atom() | unicode:charlist(),
		NEW_NODE_NAME :: node(),
		TYPE :: shortnames | longnames | nonetkernel,
		NET_KERNEL_ERROR :: not_allowed | not_found.

change_name(NEW_NAME,TYPE) when is_atom(NEW_NAME) ->

	change_name(atom_to_list(NEW_NAME),TYPE);

change_name(NEW_NAME,TYPE) when is_list(NEW_NAME) ->

	case name_type(NEW_NAME) of
		name -> change_name({node,NEW_NAME},TYPE);
		_ -> wrongnewname
	end;

change_name({node,NEW_NAME},TYPE) when is_list(NEW_NAME) ->

	case node() of
		'nonode@nohost' -> nonetkernel;
		_ -> change_name({kernel,NEW_NAME},TYPE)
	end;

change_name({kernel,NEW_NAME},TYPE) when is_list(NEW_NAME) ->

	case net_kernel:stop() of
		ok -> change_name({verified,NEW_NAME},TYPE);
		{error,REASON} -> REASON
	end;

change_name({verified,NEW_NAME},shortnames) when is_list(NEW_NAME) ->

	{ok,_NET_KERNEL_PID} = net_kernel:start([
		list_to_atom(NEW_NAME),shortnames
	]),
	{ok,node()};

change_name({verified,NEW_NAME},longnames) when is_list(NEW_NAME) ->

	{ok,_NET_KERNEL_PID} = net_kernel:start([
		list_to_atom(lists:concat([NEW_NAME,"@",net_adm:localhost()])),longnames
	]),
	{ok,node()};

change_name(_,nonetkernel) -> nonetkernel;

change_name(_,_) -> wrongtype.


%% ----------------------------
%% @doc Return list of started nodes on current server
-spec names() -> proplists:proplist().

names() ->

	HOST_NAME = fqdn(),
	[_EPMD | NODE_DESCRIPTIONS] = string:tokens(os:cmd("epmd -names"),"\n"),
	NODES = [parse_node_description(DESCRIPTION) || DESCRIPTION <- NODE_DESCRIPTIONS],
	[{list_to_atom(string:concat(string:concat(NODE_NAME,"@"),HOST_NAME)),PORT} || {NODE_NAME,PORT} <- NODES].


%% ----------------------------
%% @doc Return list of started nodes on current server
-spec names(SERVER) -> proplists:proplist()
	when SERVER :: a_host_name_string().

names(SERVER) ->

	case a_ssh:execute(SERVER,"epmd -names") of
		{true,COMMAND_OUTPUT} ->
			[_EPMD | NODE_DESCRIPTIONS] = string:tokens(COMMAND_OUTPUT,"\n"),
			NODES = [parse_node_description(DESCRIPTION) || DESCRIPTION <- NODE_DESCRIPTIONS],
			{true,[{list_to_atom(string:concat(string:concat(NODE_NAME,"@"),SERVER)),PORT} || {NODE_NAME,PORT} <- NODES]};
		{false,{SERVER,COMMAND}} ->
			{false,{SERVER,COMMAND}}
	end.


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
-spec fqdn() -> string().

fqdn() ->

	CMD_OUTPUT = os:cmd("hostname -f"),
	{HOSTNAME,_} = lists:split(length(CMD_OUTPUT) - 1, CMD_OUTPUT),
	HOSTNAME.


%% ----------------------------
%% @doc Return node name string
-spec name_string(NAME,SERVER) -> a_node_name_string()
	when
		NAME :: a_utf_text_string(),
		SERVER :: a_utf_text_string().

name_string(NAME,SERVER) -> NAME ++ "@" ++ SERVER.


%% ----------------------------
%% @doc Return node name atom
-spec name_atom(NAME,SERVER) -> a_node_name_atom()
	when
		NAME :: a_utf_text_string(),
		SERVER :: a_utf_text_string().

name_atom(NAME,SERVER) -> list_to_atom(name_string(NAME,SERVER)).


%% ----------------------------
%% @doc Return binary within MD5 node ID
-spec node_id(NODE_NAME_BINARY) -> a_id_32()
	when
		NODE_NAME_BINARY ::
			a_node_name_string() |
			a_node_name_atom() |
			a_node_name_binary().

node_id(NODE_NAME_BINARY) when is_binary(NODE_NAME_BINARY) ->

	a_sequence:md(NODE_NAME_BINARY,md5);

node_id(NODE_NAME_ATOM) when is_atom(NODE_NAME_ATOM) ->

	node_id(atom_to_binary(NODE_NAME_ATOM));

node_id(NODE_NAME_STRING) when is_list(NODE_NAME_STRING) ->

	node_id(list_to_binary(NODE_NAME_STRING)).


%% ----------------------------
%% @doc Return default node properties record
-spec default_node_properties() -> #a_node_start_properties{}.

default_node_properties() ->

	#a_node_start_properties{
		name = "test_node",
		host = fqdn(),
		detached = true,
		cookie = "no_cookie",
		port_range = false,
		port_range_min = 0,
		port_range_max = 0,
		command_timeout = 500,
		shutdown_time = 0
	}.


%% ----------------------------
%% @doc Return unix shell command for starting erlang node
-spec start_command(PARAMETERS) -> OUTPUT
	when
		PARAMETERS :: #a_node_start_properties{},
		OUTPUT :: COMMAND,
		COMMAND :: a_shell_command_string().

start_command(PARAMETERS) when is_record(PARAMETERS,a_node_start_properties) ->

	PARAMETER_NODE_NAME = string:concat(" -name ",PARAMETERS#a_node_start_properties.name),
	PARAMETER_COOKIE = string:concat(" -setcookie ",PARAMETERS#a_node_start_properties.cookie),
	PARAMETER_DETACHED = case PARAMETERS#a_node_start_properties.detached of
		true -> " -detached";
		_ -> ""
	end,
	PARAMETER_PORT_RANGE = case PARAMETERS#a_node_start_properties.port_range of
		true ->
			" -kernel " ++
			"inet_dist_listen_min " ++ PARAMETERS#a_node_start_properties.port_range_min ++ " "
			"inet_dist_listen_max " ++ PARAMETERS#a_node_start_properties.port_range_max;
		_ ->
			""
	end,
	PARAMETER_SHUTDOWN_TIME = case PARAMETERS#a_node_start_properties.shutdown_time of
		undefined -> "";
		0 -> "";
		SHUTDOWN_TIME -> " -shutdown_time " ++ integer_to_list(SHUTDOWN_TIME)
	end,

	ERL_COMMAND = "erl" ++
		PARAMETER_NODE_NAME ++ PARAMETER_COOKIE ++
		PARAMETER_DETACHED ++ PARAMETER_PORT_RANGE ++
		PARAMETER_SHUTDOWN_TIME ++
		"; echo ok;",

	ERL_COMMAND.


%% ----------------------------
%% @doc Starting node with parameters
-spec start(PARAMETERS) -> OUTPUT
	when
		PARAMETERS :: #a_node_start_properties{},
		OUTPUT :: {already_started,NODE_FULL_NAME} | {ok,NODE_FULL_NAME} | {error,REASON},
		NODE_FULL_NAME :: atom(),
		REASON :: term().

start(PARAMETERS) when is_record(PARAMETERS,a_node_start_properties) ->

	FQDN = fqdn(),
	HOST = PARAMETERS#a_node_start_properties.host,
	NODE_NAME = PARAMETERS#a_node_start_properties.name,
	NODE_FULL_NAME = NODE_NAME ++ "@" ++ HOST,

	IS_STARTED = if
		FQDN == HOST -> is_started(NODE_NAME);
		true -> is_started(NODE_NAME,HOST)
	end,
	case IS_STARTED of
		true -> {already_started,NODE_FULL_NAME};
		false ->

			ERL_COMMAND = start_command(PARAMETERS),

			CHECK_SUCCESS = fun() ->
				timer:sleep(PARAMETERS#a_node_start_properties.command_timeout),
				CHECK = if
					        FQDN == HOST -> is_started(NODE_NAME);
					        true -> is_started(NODE_NAME,HOST)
				        end,
				case CHECK of
					true -> {ok,NODE_FULL_NAME};
					_ -> {error,{ERL_COMMAND,NODE_FULL_NAME}}
				end
			end,

			START_OUTPUT = if
				FQDN == HOST -> os:cmd(ERL_COMMAND);
				true -> a_ssh:execute(HOST,ERL_COMMAND)
			end,
			case START_OUTPUT of
				"ok\n" -> CHECK_SUCCESS();
				{true,"ok\n"} -> CHECK_SUCCESS();
				_ -> {error,START_OUTPUT}
			end

	end.


%% ----------------------------
%% @doc Ensure node connected and fully operational by calling custom function
%% on this node
-spec ensure(NODE_NAME,MODULE,FUNCTION,ARGUMENTS) -> any() | {error,REASON}
	when
		NODE_NAME :: node(),
		MODULE :: module(),
		FUNCTION :: atom(),
		ARGUMENTS :: list(),
		REASON :: term().

ensure(NODE_NAME,MODULE,FUNCTION,ARGUMENTS)
	when
		is_atom(NODE_NAME),
		is_atom(MODULE),
		is_atom(FUNCTION),
		is_list(ARGUMENTS) ->

	case net_adm:ping(NODE_NAME) of
		pong -> a_rpc:async_call(NODE_NAME,MODULE,FUNCTION,ARGUMENTS);
		_ -> {error,not_connected}
	end.


%% ----------------------------
%% @doc Stopping node by FQDN
-spec stop(NODE_FQDN) -> {ok,NODE_FQDN} | {error,REASON}
	when
		NODE_FQDN :: atom() | string(),
		REASON :: term().

stop(NODE_FQDN_STRING) when is_list(NODE_FQDN_STRING) ->

	stop(list_to_atom(NODE_FQDN_STRING));

stop(NODE_FQDN) when is_atom(NODE_FQDN) ->

	case stop(NODE_FQDN,init,stop,[]) of
		ok -> {ok,NODE_FQDN};
		_ -> {error,NODE_FQDN}
	end.


%% ----------------------------
%% @doc Stopping node by calling designed function
-spec stop(NODE_FQDN,MODULE,FUNCTION,ARGUMENTS) -> any()
	when
		NODE_FQDN :: node(),
		MODULE :: module(),
		FUNCTION :: atom(),
		ARGUMENTS :: list().

stop(NODE_FQDN,MODULE,FUNCTION,ARGUMENTS)
	when
		is_atom(NODE_FQDN),
		is_atom(MODULE),
		is_atom(FUNCTION),
		is_list(ARGUMENTS) ->

	case net_adm:ping(NODE_FQDN) of
		pong ->	a_rpc:async_call(NODE_FQDN,MODULE,FUNCTION,[]);
		REPLY -> {error,{no_connected_node,REPLY}}
	end.


%% ----------------------------
%% @doc Checking if node started locally
-spec is_started(NODE_NAME) -> OUTPUT
	when
		OUTPUT :: true | false,
		NODE_NAME :: a_node_name_string().

is_started(NODE_NAME) when is_list(NODE_NAME) ->

	NODE_FULL_NAME = list_to_atom(NODE_NAME ++ "@" ++ fqdn()),
	case lists:keyfind(NODE_FULL_NAME,1,names()) of
		{NODE_FULL_NAME,_PORT_NUMBER} -> true;
		_ -> false
	end.


%% ----------------------------
%% @doc Checking if node started on server
-spec is_started(NODE_NAME,HOST) -> OUTPUT
	when
		OUTPUT :: true | false,
		NODE_NAME :: a_node_name_string(),
		HOST :: a_host_name_string().

is_started(NODE_NAME,HOST) when is_list(NODE_NAME) ->

	case names(HOST) of
		{true,NAMES} ->
			NODE_FULL_NAME = list_to_atom(NODE_NAME ++ "@" ++ HOST),
			case lists:keyfind(NODE_FULL_NAME,1,NAMES) of
				{NODE_FULL_NAME,_PORT_NUMBER} -> true;
				_ -> false
			end;
		{false,_} ->
			false
	end.


%% ----------------------------
%% @doc Check if defined node connected
-spec is_connected(NODE_NAME) -> OUTPUT
	when
		NODE_NAME :: a_node_name_string() | atom(),
		OUTPUT :: boolean().

is_connected(NODE_NAME) when is_list(NODE_NAME) ->

	is_connected(list_to_atom(NODE_NAME));

is_connected(NODE_NAME) when is_atom(NODE_NAME) ->

	lists:member(NODE_NAME,nodes()).


%% ----------------------------
%% @doc Connect to defined node
-spec connect(NODE_NAME) -> OUTPUT
	when
		NODE_NAME :: a_node_name_string() | atom(),
		OUTPUT :: {true,NODE_NAME} | {false,NODE_NAME}.

connect(NODE_NAME) when is_list(NODE_NAME) ->

	connect(list_to_atom(NODE_NAME));

connect(NODE_NAME) when is_atom(NODE_NAME) ->

	case is_connected(NODE_NAME) of
		true ->
			{true,NODE_NAME};
		false ->
			case net_adm:ping(NODE_NAME) of
				ping -> {true,NODE_NAME};
				_ -> {false,NODE_NAME}
			end
	end.