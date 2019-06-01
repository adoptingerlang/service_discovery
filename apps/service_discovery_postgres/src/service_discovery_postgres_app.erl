%%%-------------------------------------------------------------------
%% @doc service_discovery public API
%% @end
%%%-------------------------------------------------------------------
-module(service_discovery_postgres_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sdp_query:load_sql(),
    {ok, DBConfig} = application:get_env(service_discovery_postgres, db_config),
    service_discovery_postgres_sup:start_link(DBConfig).

stop(_State) ->
    ok.

