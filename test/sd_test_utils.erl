-module(sd_test_utils).

-export([random_service_name/0,
         random_name/1]).

random_service_name() ->
    random_name(<<"service-">>).

random_name(Prefix) ->
    Str = re:replace(base64:encode(crypto:strong_rand_bytes(8)), "\\W", "x",[global, {return,binary}]),
    string:casefold(<<Prefix/binary, Str/binary>>).
