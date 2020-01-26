%%%-------------------------------------------------------------------
%% @doc service_discovery public API
%% @end
%%%-------------------------------------------------------------------

-module(service_discovery_grpc_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(DEFAULT_GRPC_SERVER, #{grpc_opts => #{service_protos => [sdg_discovery_pb],
                                              services => #{'sd.DiscoveryService' => sdg_service}},
                               listen_opts => #{port => 8081,
                                                ip => {0,0,0,0}}}).

start(_StartType, _StartArgs) ->
    ServerOpts = application:get_env(service_discovery_grpc, server, ?DEFAULT_GRPC_SERVER),
    service_discovery_grpc_sup:start_link(ServerOpts).

stop(_State) ->
    ok.
