%%%-------------------------------------------------------------------
%% @doc service_discovery public API
%% @end
%%%-------------------------------------------------------------------

-module(service_discovery_grpc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    service_discovery_grpc_sup:start_link().

stop(_State) ->
    ok.
