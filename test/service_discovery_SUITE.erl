-module(service_discovery_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [initial_start_test].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(service_discovery),
    Config.

end_per_suite(_Config) ->
    application:stop(service_discovery),
    ok.

initial_start_test(_Config) ->
    ?assertMatch({service_discovery, _, _},
                 lists:keyfind(service_discovery, 1, application:which_applications())),
    ok.
