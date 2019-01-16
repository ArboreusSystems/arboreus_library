%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2016, http://arboreus.system
%%% @doc The prolists handler
%%%
%%% @end
%%% Created : 12. Февр. 2016 21:22
%%%-------------------------------------------------------------------
-module(a_proplists).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% API
-export([
	from_record/2,
	group/2
]).


%% ----------------------------
%% @doc Return proplist from record by record information
-spec from_record(Data_module,Record_source) -> proplists:proplist()
	when
	Data_module :: atom(),
	Record_source :: term().

from_record(Data_module,Record_src) when is_tuple(Record_src) ->
	[Record_name|Record] = tuple_to_list(Record_src),
	case apply(Data_module,rec_info,[Record_name]) of
		{error,Reason} -> {error,Reason};
		Record_info -> lists:zip(Record_info,Record)
	end.


%% ----------------------------
%% @doc Wrapper function for group_by_value/2 and group_by_key/2
-spec group(Action_type,Proplist) -> proplists:proplist()
	when
	Action_type :: by_value | by_key,
	Proplist :: proplists:proplist().

group(by_value,Proplist) -> group_by_value(Proplist,[]);
group(by_key,Proplist) -> group_by_key(Proplist,[]).


%% ----------------------------
%% @doc Group proplist elements by value
-spec group_by_value(Proplist,Output) -> proplists:proplist()
	when
		Proplist :: proplists:proplist(),
		Output :: proplists:proplist().

group_by_value([],Output) -> Output;
group_by_value([{Key,Value}|Proplist],Output) ->
	group_by_value(Proplist,
		case proplists:get_value(Value,Output) of
			undefined -> lists:append(Output,[{Value,[Key]}]);
			List_of_keys ->
				lists:keyreplace(Value,1,Output,{Value,lists:append(List_of_keys,[Key])})
		end
	).


%% ----------------------------
%% @doc Group proplists elements by key
-spec group_by_key(Proplist,Output) -> proplists:proplist()
	when
		Proplist :: proplists:proplist(),
		Output :: proplists:proplist().

group_by_key([],Output) -> Output;
group_by_key([{Key,Value}|Proplist],Output) ->
	group_by_key(Proplist,
		case proplists:get_value(Key,Output) of
			undefined -> lists:append(Output,[{Key,[Value]}]);
			Lists_of_values ->
				lists:keyreplace(Key,1,Output,{Key,lists:append(Lists_of_values,[Value])})
		end
	).