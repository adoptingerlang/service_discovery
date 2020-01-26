%%%-------------------------------------------------------------------
%% @doc service_discovery top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(service_discovery_grpc_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Opts) ->
    GrpcOpts = maps:get(grpc_opts, Opts, #{}),
    ServerOpts = maps:get(server_opts, Opts, #{}),
    ListenOpts = maps:get(listen_opts, Opts, #{}),
    PoolOpts = maps:get(pool_opts, Opts, #{}),
    TransportOpts = maps:get(transport_opts, Opts, #{}),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [ServerOpts, GrpcOpts, ListenOpts,
                                                      PoolOpts, TransportOpts]).

init([ServerOpts, GrpcOpts, ListenOpts, PoolOpts, TransportOpts]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 10},
    ChildSpecs = [#{id => grpcbox_services_sup,
                    start => {grpcbox_services_sup, start_link, [ServerOpts, GrpcOpts, ListenOpts,
                                                                 PoolOpts, TransportOpts]},
                    type => supervisor,
                    restart => transient,
                    shutdown => 1000}],
    {ok, {SupFlags, ChildSpecs}}.
