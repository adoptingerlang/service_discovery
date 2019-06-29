-module(service_discovery).

-export([create/1,
         lookup/1,
         lookup_endpoints/1,
         list/0,
         register/2]).

-type service() :: #{name := unicode:unicode_binary(),
                     attributes := #{unicode:unicode_binary() => unicode:unicode_binary()}}.

-type endpoint() :: #{service_name := unicode:unicode_binary(),
                      ip := inet:ip_address(),
                      port := non_neg_integer(),
                      tags := [unicode:unicode_binary()]}.

-export_type([service/0,
              endpoint/0]).

create(Service) ->
    sdp_services_storage:create(Service).

lookup(ServiceName) ->
    sdp_services_storage:read(ServiceName).

lookup_endpoints(ServiceName) ->
    sdp_services_storage:read_endpoints(ServiceName).

list() ->
    sdp_services_storage:list().

register(ServiceName, Endpoint) ->
    sdp_services_storage:register(ServiceName, Endpoint).
