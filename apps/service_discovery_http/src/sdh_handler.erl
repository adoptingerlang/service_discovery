-module(sdh_handler).

-behaviour(elli_handler).

-export([handle/2,
         handle_event/3]).

-include_lib("elli/include/elli.hrl").

-define(CONTENT_TYPE_JSON, {<<"Content-Type">>, <<"application/json">>}).

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET',[<<"services">>], Req) ->
    Services = [#{redis => #{id => <<"redis">>, port => 8080}}],
    ServicesJson = json_encode(is_pretty(Req), Services),
    {ok, [?CONTENT_TYPE_JSON], ServicesJson};

handle('GET',[<<"service">>, _Name], Req) ->
    Service = #{redis => #{id => <<"redis">>, port => 8080}},
    ServiceJson = json_encode(is_pretty(Req), Service),
    {ok, [?CONTENT_TYPE_JSON], ServiceJson};
handle('PUT',[<<"service">>, <<"register">>], _Req) ->
    {ok, [], <<>>};
handle('PUT',[<<"service">>, <<"deregister">>], _Req) ->
    {ok, [], <<>>};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

handle_event(_Event, _Data, _Args) ->
    ok.

%%

json_encode(true, ToEncode) ->
    jsx:prettify(jsx:encode(ToEncode));
json_encode(_, ToEncode) ->
    jsx:encode(ToEncode).

is_pretty(Req) ->
    case proplists:get_value(<<"pretty">>, elli_request:get_args(Req)) of
        Pretty when Pretty =:= <<"true">> ; Pretty =:= true ->
            true;
        _ ->
            false
    end.
