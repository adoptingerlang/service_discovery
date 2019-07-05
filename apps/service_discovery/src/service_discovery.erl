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

-spec create(service()) -> binary() | {error, term()}.
create(Service) ->
    sdp_services_storage:create(Service).

-spec lookup(unicode:unicode_binary()) -> service() | {error, term()}.
lookup(ServiceName) ->
    sdp_services_storage:read(ServiceName).

-spec lookup_endpoints(unicode:unicode_binary()) -> [endpoint()] | {error, term()}.
lookup_endpoints(ServiceName) ->
    sdp_services_storage:read_endpoints(ServiceName).

-spec add_named_ports(unicode:unicode_binary(), named_ports()) -> ok | {error, term()}.
add_named_ports(ServiceName, NamedPorts) ->
    sdp_services_storage:add_named_ports(ServiceName, NamedPorts).

-spec list() -> [service()] | {error, term()}.
list() ->
    sdp_services_storage:list().

-spec register(name(), endpoint()) -> ok.
register(ServiceName, Endpoint) ->
    sdp_services_storage:register(ServiceName, Endpoint).
