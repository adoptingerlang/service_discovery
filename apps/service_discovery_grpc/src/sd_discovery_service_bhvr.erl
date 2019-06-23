%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service sd.DiscoveryService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2019-06-23T17:54:08+00:00 and should not be modified manually

-module(sd_discovery_service_bhvr).

%% @doc Unary RPC
-callback get_service(ctx:ctx(), discovery_pb:get_service_request()) ->
    {ok, discovery_pb:get_service_response(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

%% @doc Unary RPC
-callback register_service(ctx:ctx(), discovery_pb:register_service_request()) ->
    {ok, discovery_pb:register_service_response(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

