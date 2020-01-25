-module(service_discovery_http_app).

-behavior(application).

-export([start/2,
         prep_stop/1,
         stop/1]).

-export([is_shutting_down/0]).

-define(IS_SHUTTING_DOWN, {?MODULE, is_shutting_down}).

start(_, _) ->
    Env = application:get_all_env(service_discovery_http),
    service_discovery_http_sup:start_link(Env).

prep_stop(State) ->
    persistent_term:put(?IS_SHUTTING_DOWN, true),
    State.

stop(_) ->
    ok.

is_shutting_down() ->
    persistent_term:get(?IS_SHUTTING_DOWN, false).
