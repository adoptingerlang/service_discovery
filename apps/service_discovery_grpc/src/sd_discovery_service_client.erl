%%%-------------------------------------------------------------------
%% @doc Client module for grpc service sd.DiscoveryService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2019-06-29T20:51:21+00:00 and should not be modified manually

-module(sd_discovery_service_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'sd.DiscoveryService').
-define(PROTO_MODULE, 'sdg_discovery_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec get_service(sdg_discovery_pb:get_service_request()) ->
    {ok, sdg_discovery_pb:get_service_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
get_service(Input) ->
    get_service(ctx:new(), Input, #{}).

-spec get_service(ctx:t() | sdg_discovery_pb:get_service_request(), sdg_discovery_pb:get_service_request() | grpcbox_client:options()) ->
    {ok, sdg_discovery_pb:get_service_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
get_service(Ctx, Input) when ?is_ctx(Ctx) ->
    get_service(Ctx, Input, #{});
get_service(Input, Options) ->
    get_service(ctx:new(), Input, Options).

-spec get_service(ctx:t(), sdg_discovery_pb:get_service_request(), grpcbox_client:options()) ->
    {ok, sdg_discovery_pb:get_service_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
get_service(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/sd.DiscoveryService/GetService">>, Input, ?DEF(get_service_request, get_service_response, <<"sd.GetServiceRequest">>), Options).

%% @doc Unary RPC
-spec create_service(sdg_discovery_pb:create_service_request()) ->
    {ok, sdg_discovery_pb:create_service_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
create_service(Input) ->
    create_service(ctx:new(), Input, #{}).

-spec create_service(ctx:t() | sdg_discovery_pb:create_service_request(), sdg_discovery_pb:create_service_request() | grpcbox_client:options()) ->
    {ok, sdg_discovery_pb:create_service_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
create_service(Ctx, Input) when ?is_ctx(Ctx) ->
    create_service(Ctx, Input, #{});
create_service(Input, Options) ->
    create_service(ctx:new(), Input, Options).

-spec create_service(ctx:t(), sdg_discovery_pb:create_service_request(), grpcbox_client:options()) ->
    {ok, sdg_discovery_pb:create_service_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
create_service(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/sd.DiscoveryService/CreateService">>, Input, ?DEF(create_service_request, create_service_response, <<"sd.CreateServiceRequest">>), Options).

%% @doc Unary RPC
-spec list_services(sdg_discovery_pb:list_services_request()) ->
    {ok, sdg_discovery_pb:list_services_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
list_services(Input) ->
    list_services(ctx:new(), Input, #{}).

-spec list_services(ctx:t() | sdg_discovery_pb:list_services_request(), sdg_discovery_pb:list_services_request() | grpcbox_client:options()) ->
    {ok, sdg_discovery_pb:list_services_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
list_services(Ctx, Input) when ?is_ctx(Ctx) ->
    list_services(Ctx, Input, #{});
list_services(Input, Options) ->
    list_services(ctx:new(), Input, Options).

-spec list_services(ctx:t(), sdg_discovery_pb:list_services_request(), grpcbox_client:options()) ->
    {ok, sdg_discovery_pb:list_services_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
list_services(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/sd.DiscoveryService/ListServices">>, Input, ?DEF(list_services_request, list_services_response, <<"sd.ListServicesRequest">>), Options).

%% @doc Unary RPC
-spec lookup_endpoints(sdg_discovery_pb:lookup_endpoints_request()) ->
    {ok, sdg_discovery_pb:lookup_endpoints_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
lookup_endpoints(Input) ->
    lookup_endpoints(ctx:new(), Input, #{}).

-spec lookup_endpoints(ctx:t() | sdg_discovery_pb:lookup_endpoints_request(), sdg_discovery_pb:lookup_endpoints_request() | grpcbox_client:options()) ->
    {ok, sdg_discovery_pb:lookup_endpoints_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
lookup_endpoints(Ctx, Input) when ?is_ctx(Ctx) ->
    lookup_endpoints(Ctx, Input, #{});
lookup_endpoints(Input, Options) ->
    lookup_endpoints(ctx:new(), Input, Options).

-spec lookup_endpoints(ctx:t(), sdg_discovery_pb:lookup_endpoints_request(), grpcbox_client:options()) ->
    {ok, sdg_discovery_pb:lookup_endpoints_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
lookup_endpoints(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/sd.DiscoveryService/LookupEndpoints">>, Input, ?DEF(lookup_endpoints_request, lookup_endpoints_response, <<"sd.LookupEndpointsRequest">>), Options).

%% @doc Unary RPC
-spec register_endpoint(sdg_discovery_pb:register_endpoint_request()) ->
    {ok, sdg_discovery_pb:register_endpoint_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
register_endpoint(Input) ->
    register_endpoint(ctx:new(), Input, #{}).

-spec register_endpoint(ctx:t() | sdg_discovery_pb:register_endpoint_request(), sdg_discovery_pb:register_endpoint_request() | grpcbox_client:options()) ->
    {ok, sdg_discovery_pb:register_endpoint_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
register_endpoint(Ctx, Input) when ?is_ctx(Ctx) ->
    register_endpoint(Ctx, Input, #{});
register_endpoint(Input, Options) ->
    register_endpoint(ctx:new(), Input, Options).

-spec register_endpoint(ctx:t(), sdg_discovery_pb:register_endpoint_request(), grpcbox_client:options()) ->
    {ok, sdg_discovery_pb:register_endpoint_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
register_endpoint(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/sd.DiscoveryService/RegisterEndpoint">>, Input, ?DEF(register_endpoint_request, register_endpoint_response, <<"sd.RegisterEndpointRequest">>), Options).

