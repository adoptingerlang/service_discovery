-module(service_discovery_http_app).

-behavior(application).

-export([start/2,
         stop/1]).

start(_, _) ->
    service_discovery_http_sup:start_link().

stop(_) ->
    ok.
