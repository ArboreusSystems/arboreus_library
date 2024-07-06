%%%-------------------------------------------------------------------
%%% @author Alexandr Kirilov, https://alexandr.kirilov.me
%%% @copyright (C) 2024, Arboreus, https://arboreus.systems
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2024 10:33
%%%-------------------------------------------------------------------
-author("Alexandr Kirilov, https://alexandr.kirilov.me").

-ifndef(A_INCLUDES).
-define(A_INCLUDES,1).

-include("constants/a_constants_general.hrl").
-include("constants/a_constants_structure_tree.hrl").

-include("macros/a_macro_lists.hrl").

-include("records/a_records_balancer.hrl").
-include("records/a_records_cluster.hrl").
-include("records/a_records_logger.hrl").
-include("records/a_records_node.hrl").
-include("records/a_records_properties.hrl").
-include("records/a_records_structure_tree.hrl").
-include("records/a_records_users.hrl").

-include("types/a_types_balancer.hrl").
-include("types/a_types_db.hrl").
-include("types/a_types_general.hrl").
-include("types/a_types_http.hrl").
-include("types/a_types_network.hrl").
-include("types/a_types_otp.hrl").
-include("types/a_types_structure_tree.hrl").
-include("types/a_types_time.hrl").
-include("types/a_types_users.hrl").
-include("types/a_types_yaws.hrl").

-endif. %% A_INCLUDES