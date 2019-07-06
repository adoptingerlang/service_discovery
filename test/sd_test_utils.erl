-module(sd_test_utils).

-export([random_service_name/0,
         random_name/1]).

random_service_name() ->
    random_name(<<"service-">>).

random_name(Prefix) ->
    <<Prefix/binary, (base64:encode(crypto:strong_rand_bytes(8)))/binary>>.
