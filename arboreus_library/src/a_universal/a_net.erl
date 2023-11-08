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

%% System include
-include("../include/types/types_network.hrl").

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
-spec ipv4_to_integer(Ip) -> ipv4_integer()
	when
	Ip :: ipv4_tuple() | ipv4_list() | ipv4_string().

ipv4_to_integer({A,B,C,D}) ->
	ipv4_to_integer([A,B,C,D]);
ipv4_to_integer([A,B,C,D])
	when
	is_integer(A),is_integer(B),
	is_integer(C),is_integer(D),
	A >= 0, A =< 255, B >= 0, B =< 255,
	C >= 0, C =< 255, D >= 0, D =< 255 ->
	(A*16777216)+(B*65536)+(C*256)+(D);
ipv4_to_integer(Ip_string) when is_list(Ip_string) ->
	{ok,Ip_tuple} = inet:parse_ipv4_address(Ip_string),
	ipv4_to_integer(Ip_tuple).


%%-----------------------------------
%% @doc Return integer from IPv6
-spec ipv6_to_integer(Ip) -> ipv4_integer() | {error,_Reason}
	when
	Ip :: ipv6_string().

ipv6_to_integer(Ip_string) ->
	{ok,_} = inet:parse_ipv6_address(Ip_string),
	list_to_integer(re:replace(Ip_string,":","",[global,{return,list}]),16).


%%-----------------------------------
%% @doc Return formatted Ip address from integer
-spec integer_to_ipv4(Integer,Output_type) ->
	ipv4_tuple() | ipv4_list() | ipv4_binary() | ipv4_string() | wrong_integer
	when
	Integer :: ipv4_integer(),
	Output_type :: tuple | list | binary | string.

integer_to_ipv4(Integer,Output_type) when is_integer(Integer), Integer >= 0 ->
	A = Integer div 16777216, After_A = Integer-A*16777216,
	B = After_A div 65536, After_B = After_A-B*65536,
	C = After_B div 256,
	D = After_B-C*256,
	if
		A > 255; B > 255; C > 255; D > 255 ->
			wrong_integer;
		true ->
			case Output_type of
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