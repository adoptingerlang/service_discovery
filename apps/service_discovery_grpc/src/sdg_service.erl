-module(sdg_service).

-behaviour(sd_discovery_service_bhvr).

-export([get_service/2,
         create_service/2,
         list_services/2,
         lookup_endpoints/2,
         register_endpoint/2]).

get_service(Ctx, #{service_name := ServiceName}) ->
    Service = service_discovery:lookup(ServiceName),
    {ok, #{service => pb_from_service(Service)}, Ctx}.

create_service(Ctx, #{service := ServicePb}) ->
    service_discovery:create(service_from_pb(ServicePb)),
    {ok, #{}, Ctx}.

list_services(Ctx, _Request) ->
    Services = service_discovery:list(),
    PbServices = pb_from_services(Services),
    {ok, #{services => PbServices}, Ctx}.

lookup_endpoints(Ctx, #{service_name := ServiceName}) ->
    Endpoints = service_discovery:lookup_endpoints(ServiceName),
    PbEndpoints = pb_from_endpoints(ServiceName, Endpoints),
    {ok, #{endpoints => PbEndpoints}, Ctx}.

register_endpoint(Ctx, #{service_name := ServiceName,
                         endpoint := EndpointPb}) ->
    {ok, Endpoint} = endpoint_from_pb(ServiceName, EndpointPb),
    ok = service_discovery:register(ServiceName, Endpoint),
    {ok, #{}, Ctx}.

%%

service_from_pb(#{name := Name,
                  attributes := Attributes}) ->
    #{name => Name,
      attributes => Attributes}.

pb_from_services(Services) ->
    pb_from_services(Services, []).

pb_from_services([], Acc) ->
    Acc;
pb_from_services([#{name := Name,
                    attributes := Attributes} | Rest], Acc) ->
    pb_from_services(Rest, [#{name => Name,
                              attributes => Attributes} | Acc]).

pb_from_service(#{name := Name,
                  attributes := Attributes}) ->
    #{name => Name,
      attributes => Attributes}.

pb_from_endpoints(ServiceName, Endpoints) ->
    pb_from_endpoints(ServiceName, Endpoints, []).

pb_from_endpoints(_ServiceName, [], Acc) ->
    Acc;
pb_from_endpoints(ServiceName, [#{ip := IP,
                                  port := Port,
                                  port_name := PortName,
                                  tags := Tags} | Rest], Acc) ->
    pb_from_endpoints(ServiceName, Rest, [#{service_name => ServiceName,
                                            ip => inet:ntoa(IP),
                                            port => Port,
                                            port_name => PortName,
                                            tags => Tags} | Acc]).

endpoint_from_pb(ServiceName, #{ip := IPString,
                                port := Port,
                                port_name := PortName,
                                tags := Tags}) ->
    case inet:parse_address(binary_to_list(IPString)) of
        {ok, IP} ->
            {ok, #{service_name => ServiceName,
                   ip => IP,
                   port => Port,
                   port_name => PortName,
                   tags => Tags}};
        {error, einval}=Error ->
            Error
    end.
