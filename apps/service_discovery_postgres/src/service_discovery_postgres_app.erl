%%%-------------------------------------------------------------------
%% @doc service_discovery public API
%% @end
%%%-------------------------------------------------------------------
-module(service_discovery_postgres_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sdp_query:load_sql(),
    service_discovery_postgres_sup:start_link().

stop(_State) ->
    ok.

