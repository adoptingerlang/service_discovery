%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service sd.DiscoveryService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2019-07-05T14:33:23+00:00 and should not be modified manually

-module(sd_discovery_service_bhvr).

%% @doc Unary RPC
-callback get_service(ctx:ctx(), sdg_discovery_pb:get_service_request()) ->
    {ok, sdg_discovery_pb:get_service_response(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

%% @doc Unary RPC
-callback create_service(ctx:ctx(), sdg_discovery_pb:create_service_request()) ->
    {ok, sdg_discovery_pb:create_service_response(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

%% @doc Unary RPC
-callback list_services(ctx:ctx(), sdg_discovery_pb:list_services_request()) ->
    {ok, sdg_discovery_pb:list_services_response(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

%% @doc Unary RPC
-callback add_named_ports(ctx:ctx(), sdg_discovery_pb:add_named_ports_request()) ->
    {ok, sdg_discovery_pb:add_named_ports_response(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

%% @doc Unary RPC
-callback lookup_endpoints(ctx:ctx(), sdg_discovery_pb:lookup_endpoints_request()) ->
    {ok, sdg_discovery_pb:lookup_endpoints_response(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

%% @doc Unary RPC
-callback register_endpoint(ctx:ctx(), sdg_discovery_pb:register_endpoint_request()) ->
    {ok, sdg_discovery_pb:register_endpoint_response(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

