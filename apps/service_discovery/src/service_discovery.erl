-module(service_discovery).

-export([create/1,
         lookup/1,
         lookup_endpoints/1,
         add_named_ports/2,
         list/0,
         register/2]).

-type name() :: unicode:unicode_binary().
-type named_ports() :: #{name() => #{protocol := name(),
                                     port := non_neg_integer()}}.

-type attributes() :: #{unicode:unicode_binary() => unicode:unicode_binary()}.
-type service() :: #{name := name(),
                     attributes := attributes(),
                     endpoints => [endpoint()],
                     named_ports => named_ports()}.

-type tag() :: unicode:unicode_binary().
-type endpoint() :: #{service_name := name(),
                      ip := inet:ip_address(),
                      tags := [tag()]}.

-export_type([service/0,
              named_ports/0,
              endpoint/0,
              tag/0,
              attributes/0]).

create(Service) ->
    sdp_services_storage:create(Service).

lookup(ServiceName) ->
    sdp_services_storage:read(ServiceName).

lookup_endpoints(ServiceName) ->
    sdp_services_storage:read_endpoints(ServiceName).

add_named_ports(ServiceName, NamedPorts) ->
    sdp_services_storage:add_named_ports(ServiceName, NamedPorts).

list() ->
    sdp_services_storage:list().

register(ServiceName, Endpoint) ->
    sdp_services_storage:register(ServiceName, Endpoint).
