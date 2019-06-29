-module(sdp_services_storage).

-export([create/1,
         register/2,
         list/0,
         read/1,
         read_endpoints/1]).

-spec create(service_discovery:service()) -> ok | {error, term()}.
create(Service) ->
    case sdp_query:run(insert_service, service_to_row(Service)) of
        #{command := insert,
          num_rows := 1} ->
            ok;
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

-spec read(unicode:unicode_binary()) -> service_discovery:service().
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

-spec read_endpoints(unicode:unicode_binary()) -> [service_discovery:endpoint()] | {error, term()}.
read_endpoints(ServiceName) ->
    case sdp_query:run(select_endpoints, [ServiceName]) of
        #{rows := Rows} ->
            [endpoint_from_row(ServiceName, Row) || Row <- Rows];
        {error, Reason} ->
            {error, Reason}
    end.

service_from_row({Name, Attributes}) ->
    #{name => Name,
      attributes => Attributes}.

endpoint_from_row(ServiceName, {IP, Port, Tags}) ->
    #{service_name => ServiceName,
      ip => IP,
      port => Port,
      tags => Tags}.

service_to_row(#{name := Name,
                 attributes := Attributes}) ->
    [Name, Attributes].

endpoint_to_row(ServiceName, #{ip := IP,
                               port := Port,
                               tags := Tags}) ->
    [ServiceName, IP, Port, Tags].
