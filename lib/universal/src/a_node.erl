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

%% Constants

%% Data types
-include("../../data_models/types/types_general.hrl").
-include("../../data_models/types/types_a_balancer.hrl").

%% Data models
-include("../../data_models/records/records_a_balancer.hrl").

%% API
-export([
	test/0,
	change_name/1,change_name/2,
	name_type/0,name_type/1,
	parse_name/0,parse_name/1,
	load/1
	
]).



%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_node) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Load_list_structure = [(fun is_integer/1),(fun is_integer/1),(fun is_integer/1)],
	Load_list = load(list),
	Load_list_wrong = [wrong,list,structure],
	true = a_structure_l:verify(return_boolean,Load_list_structure,Load_list),
	false = a_structure_l:verify(return_boolean,Load_list_structure,Load_list_wrong),
	Load_record_structure = {
		(fun is_atom/1),
		(fun is_integer/1),
		(fun is_integer/1),
		(fun is_integer/1)
	},
	Load_record = load(record),
	Load_record_wrong = #a_node_load{
		processes = processes,
		memory_total = memory_total,
		ports = ports
	},
	true = a_structure_r:verify(return_boolean,Load_record_structure,Load_record),
	false = a_structure_r:verify(return_boolean,Load_record_structure,Load_record_wrong),
	io:format("DONE! Fun load/1 test passed~n"),
	New_name_sting = "new_name",
	Change_name_test = case node() of
		'nonode@nohost' ->
			nonetkernel = change_name(test),
			Old_name_string = "old_name",
			{ok,_} = net_kernel:start([list_to_atom(Old_name_string)]),
			{ok,{Old_name_string,Domain}} = parse_name(),
			Old_name = node(),
			{ok,New_name} = change_name(list_to_atom(New_name_sting)),
			{ok,{New_name_sting,Domain}} = parse_name(New_name),
			{ok,Old_name} = change_name(list_to_atom(Old_name_string)),
			net_kernel:stop();
		Old_node_name ->
			{ok,{Old_name_string,Domain}} = parse_name(),
			case change_name(list_to_atom(New_name_sting)) of
				{ok,New_name} ->
					{ok,{New_name_sting,Domain}} = parse_name(New_name),
					{ok,Old_node_name} = change_name(list_to_atom(Old_name_string)),
					ok;
				Result -> Result
			end
	end,
	io:format("DONE! Change name functionality test result: ~p~n",[Change_name_test]),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_node) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Node load
-spec load(Kind) -> list_of_properties() | a_node_load()
	when
	Kind :: list | record.

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
-spec parse_name() -> {ok,{Name,Domain}} | nonetkernel
	when
	Name :: unicode:charlist(),
	Domain :: unicode:charlist().

parse_name() -> parse_name(node()).


%% ----------------------------
%% @doc Parse node name
-spec parse_name(Node_name) -> {ok,{Name,Domain}} | {ok,Name} | nomatch | nonetkernel
	when
	Node_name :: node() | unicode:charlist(),
	Name :: unicode:charlist(),
	Domain :: unicode:charlist().

parse_name('nonode@nohost') -> nonetkernel;
parse_name(Node_name) when is_atom(Node_name) ->
	parse_name(atom_to_list(Node_name));
parse_name(Node_name) when is_list(Node_name) ->
	case re:run(Node_name,"^([a-zA-Z0-9\_\-]{1,})\@([a-zA-Z0-9\_\-]{1,}\.{0,1}){1,}$") of
		{match,_} ->
			[Name,Domain] = string:tokens(Node_name,"@"),
			{ok,{Name,Domain}};
		_ ->
			case re:run(Node_name,"^([a-zA-Z0-9\-\_]{1,})$") of
				{match,_} -> {ok,Node_name};
				_ -> nomatch
			end
	end.


%% ----------------------------
%% @doc Get current node name type
-spec name_type() -> nonetkernel | shortnames | longnames.

name_type() -> name_type(node()).


%% ----------------------------
%% @doc Return node name type
-spec name_type(Name) -> shortnames | longnames | name | nomatch | nonetkernel
	when
	Name :: atom() | unicode:charlist().

name_type('nonode@nohost') -> nonetkernel;
name_type(Name) when is_atom(Name) ->
	name_type(atom_to_list(Name));
name_type(Name) when is_list(Name) ->
	case re:run(Name,"^[a-zA-Z0-9\_\-]{1,}\@[a-zA-Z0-9\_\-]{1,}$") of
		{match,_} -> shortnames;
		_ ->
			case re:run(Name,"^[a-zA-Z0-9\_\-]{1,}\@[a-zA-Z0-9\.\_\-]{1,}$") of
				{match,_} -> longnames;
				_ ->
					case re:run(Name,"^[a-zA-Z0-9\_\-]{1,}$") of
						{match,_} -> name;
						_ -> nomatch
					end
			end
	end.


%% ----------------------------
%% @doc Change node name
-spec change_name(New_name) -> {ok,New_node_name} | wrongnewname | wrongtype | nonetkernel
	when
	New_name :: atom() | unicode:charlist(),
	New_node_name :: node().

change_name(New_name) ->
	change_name(New_name,name_type()).


%% ----------------------------
%% @doc Change node name
-spec change_name(New_name,Type) ->
	{ok,New_node_name} | wrongnewname | wrongtype | nonetkernel | Net_kernel_error
	when
	New_name :: atom() | unicode:charlist(),
	New_node_name :: node(),
	Type :: shortnames | longnames | nonetkernel,
	Net_kernel_error :: not_allowed | not_found.

change_name(New_name,Type) when is_atom(New_name) ->
	change_name(atom_to_list(New_name),Type);
change_name(New_name,Type) when is_list(New_name) ->
	case name_type(New_name) of
		name -> change_name({node,New_name},Type);
		_ -> wrongnewname
	end;
change_name({node,New_name},Type) when is_list(New_name) ->
	case node() of
		'nonode@nohost' -> nonetkernel;
		_ -> change_name({kernel,New_name},Type)
	end;
change_name({kernel,New_name},Type) when is_list(New_name) ->
	case net_kernel:stop() of
		ok -> change_name({verified,New_name},Type);
		{error,Reason} -> Reason
	end;
change_name({verified,New_name},shortnames) when is_list(New_name) ->
	{ok,_Net_kernel_pid} = net_kernel:start([
		list_to_atom(New_name),shortnames
	]),
	{ok,node()};
change_name({verified,New_name},longnames) when is_list(New_name) ->
	{ok,_Net_kernel_pid} = net_kernel:start([
		list_to_atom(lists:concat([New_name,"@",net_adm:localhost()])),longnames
	]),
	{ok,node()};
change_name(_,nonetkernel) -> nonetkernel;
change_name(_,_) -> wrongtype.