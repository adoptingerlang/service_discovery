-module(sdg_service).

-behaviour(sd_discovery_service_bhvr).

-export([get_service/2,
         list_services/2,
         register_service/2]).

get_service(Ctx, _Request) ->
    Response = #{},
    {ok, Response, Ctx}.

list_services(Ctx, _Request) ->
    Response = #{services => [#{id => <<"redis">>, port => 5432}]},
    {ok, Response, Ctx}.

register_service(Ctx, _Request) ->
    Response = #{},
    {ok, Response, Ctx}.
