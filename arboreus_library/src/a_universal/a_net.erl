%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2016, http://arboreus.system
%%% @doc Network functionality handler
%%%
%%% @end
%%% Created : 19. Янв. 2016 17:47
%%%-------------------------------------------------------------------
-module(a_net).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Data types
-include_lib("../include/types/types_a_general.hrl").
-include_lib("../include/types/types_a_network.hrl").

%% API
-export([
	test/0,
	ipv4_to_integer/1,
	ipv6_to_integer/1,
	integer_to_ipv4/2
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%%-----------------------------------
%% @doc Return integer from IPv4
-spec ipv4_to_integer(IP) -> a_ipv4_integer()
	when
		IP :: a_ipv4_tuple() | a_ipv4_list() | a_ipv4_string().

ipv4_to_integer({A,B,C,D}) -> ipv4_to_integer([A,B,C,D]);

ipv4_to_integer([A,B,C,D])
	when
		is_integer(A),is_integer(B),
		is_integer(C),is_integer(D),
		A >= 0, A =< 255, B >= 0, B =< 255,
		C >= 0, C =< 255, D >= 0, D =< 255 ->

	(A*16777216)+(B*65536)+(C*256)+(D);

ipv4_to_integer(IP_STRING) when is_list(IP_STRING) ->

	{ok,IP_TUPLE} = inet:parse_ipv4_address(IP_STRING),
	ipv4_to_integer(IP_TUPLE).


%%-----------------------------------
%% @doc Return integer from IPv6
-spec ipv6_to_integer(IP) -> a_ipv4_integer() | {error,REASON}
	when
		IP :: a_ipv6_string(),
		REASON :: term().

ipv6_to_integer(IP_STRING) ->

	case inet:parse_ipv6_address(IP_STRING) of
		{ok,_IP_V6_ADDRESS} ->
			list_to_integer(re:replace(IP_STRING,":","",[global,{return,list}]),16);
		{error,REASON} ->
			{error,REASON}
	end.


%%-----------------------------------
%% @doc Return formatted Ip address from integer
-spec integer_to_ipv4(INTEGER,OUTPUT_TYPE) ->
	a_ipv4_tuple() | a_ipv4_list() | a_ipv4_binary() | a_ipv4_string() | wrong_integer
	when
		INTEGER :: a_ipv4_integer(),
		OUTPUT_TYPE :: tuple | list | binary | string.

integer_to_ipv4(INTEGER,OUTPUT_TYPE) when is_integer(INTEGER), INTEGER >= 0 ->

	A = INTEGER div 16777216, AFTER_A = INTEGER - A * 16777216,
	B = AFTER_A div 65536, AFTER_B = AFTER_A - B * 65536,
	C = AFTER_B div 256,
	D = AFTER_B - C * 256,

	if
		A > 255; B > 255; C > 255; D > 255 ->
			wrong_integer;
		true ->
			case OUTPUT_TYPE of
				tuple -> {A,B,C,D};
				list -> [A,B,C,D];
				binary ->
					<<(integer_to_binary(A))/binary,"."/utf8,
						(integer_to_binary(B))/binary,"."/utf8,
						(integer_to_binary(C))/binary,"."/utf8,
						(integer_to_binary(D))/binary>>;
				_ ->
					lists:concat([
						integer_to_list(A),".",
						integer_to_list(B),".",
						integer_to_list(C),".",
						integer_to_list(D)
					])
			end
	end.