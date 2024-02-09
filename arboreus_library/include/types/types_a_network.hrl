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

-type a_ipv4_byte() :: a_byte_8().
-type a_ipv4_string() :: string().
-type a_ipv4_binary() :: bitstring().
-type a_ipv4_tuple() :: {a_ipv4_byte(),a_ipv4_byte(),a_ipv4_byte(),a_ipv4_byte()}.
-type a_ipv4_list() :: [a_ipv4_byte()].
-type a_ipv4_integer() :: 0..4294967295.

-type a_ipv6_byte() :: a_byte_16().
-type a_ipv6_string() :: string().
-type a_ipv6_binary() :: bitstring().
-type a_ipv6_tuple() :: {
	a_ipv6_byte(),a_ipv6_byte(),a_ipv6_byte(),a_ipv6_byte(),
	a_ipv6_byte(),a_ipv6_byte(),a_ipv6_byte(),a_ipv6_byte()
}.
-type a_ipv6_list() :: [a_ipv6_byte()].
-type a_ipv6_integer() :: 0..340282366920938463463374607431768211455.

-type a_host_name_string() :: a_utf_text_string().
-type a_host_name_binary() :: a_utf_text_binary().

-type a_node_name_string() :: a_utf_text_string().
-type a_node_name_binary() :: a_utf_text_binary().

-type a_node_cookie() :: atom().
-type a_node_cookie_string() :: a_utf_base64_string().
-type a_node_cookie_binary() :: a_utf_text_binary().

-type a_network_user() :: a_network_user_string() | a_network_user_binary().
-type a_network_user_string() :: a_utf_text_string().
-type a_network_user_binary() :: a_utf_text_binary().

-type a_network_password() :: a_network_password_string() | a_network_password_binary().
-type a_network_password_string() :: a_utf_text_string().
-type a_network_password_binary() :: a_utf_text_binary().

-type a_network_user_credentials() :: {a_network_user(),a_network_password()}.

-type a_port() :: 0..65353.

-type a_url_string() :: a_utf_text_string().
-type a_url_binary() :: a_utf_text_binary().
-type a_url_scheme_string() :: a_utf_text_string().
-type a_url_scheme_binary() :: a_utf_text_binary().
-type a_url_path_string() :: a_utf_text_string().
-type a_url_path_binary() :: a_utf_text_binary().