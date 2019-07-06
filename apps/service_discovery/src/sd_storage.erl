-module(sd_storage).

-export([configure_storage/1,
         create/1,
         read/1,
         read_endpoints/1,
         add_named_ports/2,
         list/0,
         register/2]).

-define(STORAGE_KEY, {?MODULE, storage_module}).
-define(STORAGE_MOD, (persistent_term:get(?STORAGE_KEY))).

-callback create(service_discovery:service()) -> binary() | {error, term()}.
-callback read(unicode:unicode_binary()) -> service_discovery:service() | {error, term()}.
-callback read_endpoints(unicode:unicode_binary()) -> [service_discovery:endpoint()] | {error, term()}.
-callback add_named_ports(unicode:unicode_binary(), service_discovery:named_ports()) -> ok | {error, term()}.
-callback list() -> [service_discovery:service()] | {error, term()}.
-callback register(service_discovery:name(), service_discovery:endpoint()) -> ok | {error, term()}.

configure_storage(StorageMod) ->
    persistent_term:put(?STORAGE_KEY, StorageMod).

-spec create(service_discovery:service()) -> binary() | {error, term()}.
create(Service) ->
    ?STORAGE_MOD:create(Service).

-spec read(unicode:unicode_binary()) -> service_discovery:service() | {error, term()}.
read(ServiceName) ->
    ?STORAGE_MOD:read(ServiceName).

-spec read_endpoints(unicode:unicode_binary()) -> [service_discovery:endpoint()] | {error, term()}.
read_endpoints(ServiceName) ->
    ?STORAGE_MOD:read_endpoints(ServiceName).

-spec add_named_ports(unicode:unicode_binary(), service_discovery:named_ports()) -> ok | {error, term()}.
add_named_ports(ServiceName, NamedPorts) ->
    ?STORAGE_MOD:add_named_ports(ServiceName, NamedPorts).

-spec list() -> [service_discovery:service()] | {error, term()}.
list() ->
    ?STORAGE_MOD:list().

-spec register(service_discovery:name(), service_discovery:endpoint()) -> ok.
register(ServiceName, Endpoint) ->
    ?STORAGE_MOD:register(ServiceName, Endpoint).
