%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Data types for Arboreus library: Network types
%%%
%%% @end
%%% Created : 07. Янв. 2018 22:59
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").


%% ------------------------------------------
%% Network types

-type ipv4_byte() :: a_byte_8().
-type ipv4_string() :: string().
-type ipv4_binary() :: bitstring().
-type ipv4_tuple() :: {ipv4_byte(),ipv4_byte(),ipv4_byte(),ipv4_byte()}.
-type ipv4_list() :: [ipv4_byte()].
-type ipv4_integer() :: 0..4294967295.

-type ipv6_byte() :: a_byte_16().
-type ipv6_string() :: string().
-type ipv6_binary() :: bitstring().
-type ipv6_tuple() :: {
	ipv6_byte(),ipv6_byte(),ipv6_byte(),ipv6_byte(),
	ipv6_byte(),ipv6_byte(),ipv6_byte(),ipv6_byte()
}.
-type ipv6_list() :: [ipv6_byte()].
-type ipv6_integer() :: 0..340282366920938463463374607431768211455.