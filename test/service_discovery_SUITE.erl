-module(service_discovery_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [create_service].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(service_discovery_postgres),
    Config.

end_per_suite(_Config) ->
    application:stop(service_discovery_postgres),
    ok.

create_service(_Config) ->
    ServiceName = sd_test_utils:random_service_name(),
    ServiceAttributes = #{<<"test-key-1">> => <<"test-value-1">>},
    _Uuid = service_discovery:create(#{name => ServiceName,
                                       attributes => ServiceAttributes}),
    ?assertMatch(#{name := ServiceName,
                   attributes := ServiceAttributes}, service_discovery:lookup(ServiceName)),

    service_discovery:register(ServiceName, #{ip => {127,0,0,1},
                                              tags => []}),
    ?assertMatch([{127,0,0,1}], dns_a_lookup(ServiceName)),

    ok.

%%

dns_a_lookup(ServiceName) ->
    inet_res:lookup(binary_to_list(ServiceName), in, a, [{nameservers, [{{127,0,0,1}, 8053}]}]).
