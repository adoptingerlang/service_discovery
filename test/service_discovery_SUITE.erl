-module(service_discovery_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [create_service].

suite() ->
    %% don't use compose hook in circleci
    case os:getenv("CI") of
        false ->
            %% this won't work until it can properly wait for the db migration to complete
            %% [{ct_hooks, [docker_compose_cth]}];
            [];
        _ ->
            []
    end.

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
    ?assertMatch([{127,0,0,2}], dns_a_lookup(ServiceName)),

    ok.

%%

dns_a_lookup(ServiceName) ->
    inet_res:lookup(binary_to_list(ServiceName), in, a, [{nameservers, [{{127,0,0,1}, 8053}]}]).
