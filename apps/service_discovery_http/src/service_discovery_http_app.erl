-module(service_discovery_http_app).

-behavior(application).

-export([start/2,
         stop/1]).

start(_, _) ->
    Env = application:get_all_env(service_discovery_http),
    service_discovery_http_sup:start_link(Env).

stop(_) ->
    ok.

