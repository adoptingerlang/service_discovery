-module(sdp_services_storage).

-export([create/1,
         register/2,
         add_named_ports/2,
         list/0,
         read/1,
         read_service_and_endpoints/1,
         read_endpoints/1]).

-spec create(service_discovery:service()) -> binary() | {error, term()}.
create(Service) ->
    case sdp_query:run(insert_service, service_to_row(Service)) of
        #{command := insert,
          num_rows := 1,
          rows := [{ServiceId}]} ->
            ServiceId;
        {error, _Reason}=Error ->
            Error
    end.

-spec register(unicode:unicode_binary(), service_discovery:endpoint()) -> ok | {error, term()}.
register(ServiceName, Endpoint) ->
    case sdp_query:run(insert_endpoint, endpoint_to_row(ServiceName, Endpoint)) of
        #{command := insert,
          num_rows := 1} ->
            ok;
        {error, _Reason}=Error ->
            Error
    end.

-spec add_named_ports(unicode:unicode_binary(), service_discovery:named_ports()) -> ok | {error, term()}.
add_named_ports(ServiceName, NamedPorts) ->
    case sdp_query:run(insert_named_ports, named_ports_to_rows(ServiceName, NamedPorts)) of
        #{command := insert,
          num_rows := _N} ->
            ok;
        {error, _Reason}=Error ->
            Error
    end.

-spec read(unicode:unicode_binary()) -> service_discovery:service() | {error, term()}.
read(ServiceName) ->
    case sdp_query:run(select_service, [ServiceName]) of
        #{rows := [Row]} ->
            service_from_row(Row);
        #{rows := []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

-spec list() -> [service_discovery:service()].
list() ->
    case sdp_query:run(select_all_services, []) of
        #{rows := Rows} ->
            [service_from_row(Row) || Row <- Rows];
        {error, Reason} ->
            {error, Reason}
    end.

-spec read_service_and_endpoints(unicode:unicode_binary()) -> {service_discovery:service(),
                                                               [service_discovery:endpoint()]} |
                                                              {error, term()}.
read_service_and_endpoints(ServiceName) ->
    case sdp_query:run(select_endpoints, [ServiceName]) of
        #{rows := Rows} ->
            [endpoint_from_row(ServiceName, Row) || Row <- Rows];
        {error, Reason} ->
            {error, Reason}
    end.

-spec read_endpoints(unicode:unicode_binary()) -> [service_discovery:endpoint()] | {error, term()}.
read_endpoints(ServiceName) ->
    case sdp_query:run(select_endpoints, [ServiceName]) of
        #{rows := Rows} ->
            [endpoint_from_row(ServiceName, Row) || Row <- Rows];
        {error, Reason} ->
            {error, Reason}
    end.

%%

service_from_row({Name, Attributes, NamedPorts, Endpoints}) ->
    #{name => Name,
      attributes => Attributes,
      named_ports => named_ports_from_rows(NamedPorts),
      endpoints => [endpoint_from_row(Name, Endpoint) || Endpoint <- Endpoints]}.

named_ports_to_rows(ServiceName, NamePorts) ->
    [ServiceName, maps:fold(fun(PortName, #{protocol := Protocol,
                                            port := Port}, Acc) ->
                                    [{<<>>, PortName, Protocol, Port} | Acc]
                            end, [], NamePorts)].

named_ports_from_rows(NamedPorts) ->
    named_ports_from_rows(NamedPorts, #{}).

named_ports_from_rows([], Acc) ->
    Acc;
named_ports_from_rows([{_, Protocol, Name, Port} | H], Acc) ->
    named_ports_from_rows(H, Acc#{Name => #{protocol => Protocol,
                                            port => Port}}).

endpoint_from_row(ServiceName, {IP, Tags}) ->
    #{service_name => ServiceName,
      ip => IP,
      tags => Tags};
endpoint_from_row(ServiceName, {_, IP, Tags, _, _}) ->
    #{service_name => ServiceName,
      ip => IP,
      tags => Tags}.

service_to_row(#{name := Name,
                 attributes := Attributes}) ->
    [Name, Attributes].

endpoint_to_row(ServiceName, #{ip := IP,
                               tags := Tags}) ->
    [ServiceName, IP, Tags].
