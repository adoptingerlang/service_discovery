-module(service_discovery_storage).

-export([]).

-callback create(service_discovery:service()) -> binary() | {error, term()}.
-callback read(unicode:unicode_binary()) -> service_discovery:service() | {error, term()}.
-callback read_endpoints(unicode:unicode_binary()) -> [service_discovery:endpoint()] | {error, term()}.
-callback add_named_ports(unicode:unicode_binary(), service_discovery:named_ports()) -> ok | {error, term()}.
-callback list() -> [service_discovery:service()] | {error, term()}.
-callback register(service_discovery:name(), service_discovery:endpoint()) -> ok | {error, term()}.
