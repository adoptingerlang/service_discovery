-module(sdh_handler).

-behaviour(elli_handler).

-export([handle/2,
         handle_event/3]).

-include_lib("elli/include/elli.hrl").

-define(CONTENT_TYPE_JSON, {<<"Content-Type">>, <<"application/json">>}).

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET',[<<"services">>], Req) ->
    Services = service_discovery:list(),
    ServicesJson = json_encode_services(is_pretty(Req), Services),
    {ok, [?CONTENT_TYPE_JSON], ServicesJson};

handle('GET',[<<"service">>, ServiceName], Req) ->
    Service = service_discovery:lookup(ServiceName),
    ServiceJson = json_encode_service(is_pretty(Req), Service),
    {ok, [?CONTENT_TYPE_JSON], ServiceJson};
handle('PUT',[<<"service">>], Req) ->
    Body = elli_request:body(Req),
    DecodedBody = jsx:decode(Body, [return_maps]),
    case create_service(DecodedBody) of
        {error, Reason} ->
            {400, [], io_lib:format("error: ~p", [Reason])};
        ServiceId ->
            {ok, [], uuid:uuid_to_string(ServiceId, binary_standard)}
    end;

handle('GET',[<<"service">>, ServiceName, <<"endpoints">>], Req) ->
    Endpoints = service_discovery:lookup_endpoints(ServiceName),
    EndpointsJson = json_encode_endpoints(is_pretty(Req), Endpoints),
    {ok, [?CONTENT_TYPE_JSON], EndpointsJson};
handle('PUT',[<<"service">>, ServiceName, <<"port">>], Req) ->
    Body = elli_request:body(Req),
    DecodedBody = jsx:decode(Body, [return_maps]),
    case add_named_port(ServiceName, DecodedBody) of
        ok ->
            {ok, [], <<>>};
        {error, Reason} ->
            {400, [], io_lib:format("error: ~p", [Reason])}
    end;
handle('PUT',[<<"service">>, ServiceName, <<"register">>], Req) ->
    Body = elli_request:body(Req),
    DecodedBody = jsx:decode(Body, [return_maps]),
    case register_service(ServiceName, DecodedBody) of
        ok ->
            {ok, [], <<>>};
        {error, Reason} ->
            {400, [], io_lib:format("error: ~p", [Reason])}
    end;
handle('PUT',[<<"service">>, <<"deregister">>, _ServiceId], _Req) ->
    {ok, [], <<>>};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

handle_event(_Event, _Data, _Args) ->
    ok.

%%

create_service(ServiceJson) ->
    case service_from_json(ServiceJson) of
        {error, _}=Error ->
            Error;
        Service ->
            service_discovery:create(Service)
    end.

-spec json_encode_service(boolean(), service_discovery:service()) -> binary().
json_encode_service(Pretty, Service=#{name := _Name,
                                      attributes := _Attributes,
                                      named_ports := _NamedPorts}) ->
    json_encode(Pretty, Service).

-spec json_encode_services(boolean(), [service_discovery:service()]) -> binary().
json_encode_services(Pretty, Services) ->
    json_encode(Pretty, json_encode_services_(Services, [])).

json_encode_services_([], Acc)  ->
    Acc;
json_encode_services_([#{name := _ServiceName,
                         attributes := _Attributes,
                         named_ports := _NamedPorts}=Service | Rest], Acc) ->
    json_encode_services_(Rest, [Service | Acc]).

-spec json_encode_endpoints(boolean(), [service_discovery:endpoints()]) -> binary().
json_encode_endpoints(Pretty, Endpoints) ->
    json_encode(Pretty, json_encode_endpoints_(Endpoints, [])).

-spec json_encode_endpoints_([service_discovery:endpoint()], [map()]) -> [map()].
json_encode_endpoints_([], Acc)  ->
    Acc;
json_encode_endpoints_([#{service_name := ServiceName,
                          ip := IP,
                          tags := Tags} | Rest], Acc) ->
    case inet:ntoa(IP) of
        {error, einval} ->
            %% add log
            json_encode_endpoints_(Rest, Acc);
        IPString ->
            json_encode_endpoints_(Rest, [#{service_name => ServiceName,
                                            ip => list_to_binary(IPString),
                                            tags => Tags} | Acc])
    end.


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

%% decoded json to service or endpoint maps

service_from_json(#{<<"name">> := Name,
                    <<"attributes">> := Attributes,
                    <<"named_ports">> := NamedPorts}) ->
    #{name => Name,
      attributes => Attributes,
      named_ports => named_ports_from_json(NamedPorts)};
service_from_json(#{<<"name">> := Name,
                    <<"attributes">> := Attributes}) ->
    #{name => Name,
      attributes => Attributes,
      named_ports => #{}};
service_from_json(_) ->
    {error, bad_service_json}.

-spec named_ports_from_json(map()) -> service_discovery:named_ports().
named_ports_from_json(Map) ->
    maps:fold(fun(K, #{<<"protocol">> := Protocol,
                       <<"port">> := Port}, Acc) ->
                      Acc#{K => #{protocol => Protocol,
                                  port => Port}}
              end, #{}, Map).

add_named_port(ServiceName, Json) ->
    case named_ports_from_json(Json) of
        {ok, NamedPorts} ->
            service_discovery:add_named_port(ServiceName, NamedPorts);
        {error, _Reason}=Error ->
            Error
    end.

register_service(ServiceName, Json) ->
    case endpoint_from_json(ServiceName, Json) of
        {ok, Endpoint} ->
            service_discovery:register(ServiceName, Endpoint);
        {error, _Reason}=Error ->
            Error
    end.

-spec endpoint_from_json(unicode:unicode_binary(), map()) -> {ok, service_discovery:endpoint()} |
                                                             {error, term()}.
endpoint_from_json(ServiceName, #{<<"ip">> := IPString,
                                  <<"tags">> := Tags}) ->
    case inet:parse_address(binary_to_list(IPString)) of
        {ok, IP} ->
            {ok, #{service_name => ServiceName,
                   ip => IP,
                   tags => Tags}};
        {error, einval}=Error ->
            Error
    end;
endpoint_from_json(_, _) ->
    {error, bad_endpoint_json}.

