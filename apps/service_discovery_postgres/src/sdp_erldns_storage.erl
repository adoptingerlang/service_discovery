-module(sdp_erldns_storage).

-include_lib("erldns/include/erldns.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("dns/include/dns.hrl").

%% API
-export([create/1,
         insert/2,
         delete_table/1,
         delete/2,
         backup_table/1,
         backup_tables/0,
         select/2,
         select/3,
         foldl/3,
         empty_table/1,
         list_table/1]).

-define(SOA(Key), [#dns_rr{name = Key,
                           type = ?DNS_TYPE_SOA,
                           ttl = 3600,
                           data = #dns_rrdata_soa{mname = <<"ns1.", Key/binary>>,
                                                  rname = <<"admin.", Key/binary>>,
                                                  serial = 2013022001,
                                                  refresh = 86400,
                                                  retry = 7200,
                                                  expire = 604800,
                                                  minimum = 300}}]).

-spec create(atom()) -> ok.
create(schema) ->
    ok;
create(zones) ->
    ok;
create(authorities) ->
    ok.

-spec insert(atom(), any()) -> any().
insert(_, _) ->
    ok.

-spec delete_table(atom()) -> ok | {aborted, any()}.
delete_table(_Table) ->
    ok.

-spec delete(Table :: atom(), Key :: term()) -> ok | any().
delete(_Table, _Key)->
    ok.

-spec backup_table(atom()) -> ok | {error, Reason :: term()}.
backup_table(_Table)->
    ok.

-spec backup_tables() -> ok | {error, Reason :: term()}.
backup_tables()->
    ok.

-spec select(Table :: atom(), Key :: term()) -> [tuple()] | {error, not_implemented}.
select(zones, Key)->
    Service = string:split(Key, ".", all) -- [<<"svc">>, <<"cluster">>, <<"local">>],
    case sdp_services_storage:read(Service) of
        {error, not_found}->
           [];
        #{named_ports := NamedPorts,
         endpoints := Endpoints} ->
            Version = <<>>,
            Authorities = ?SOA(Key),
            SrvRecords = named_ports_to_srv_records(Key, NamedPorts),
            Records = SrvRecords ++ endpoints_to_a_records(Key, Endpoints),
            [{Key, #zone{name=Key,
                         version=Version,
                         authority=Authorities,
                         record_count = length(Records),
                         records=Records,
                         records_by_name=erldns_zone_cache:build_named_index(Records),
                         keysets=[]}}]
    end;
select(_Table, _Key)->
    {error, not_implemented}.

-spec select(atom(), list(), infinite | integer()) -> [tuple()] | {error, not_implemented}.
select(_Table, _MatchSpec, _Limit) ->
    {error, not_implemented}.

-spec foldl(fun(), list(), atom())  -> Acc :: term() | {error, Reason :: term()}.
foldl(_Iterator, _Acc, _Table) ->
    {error, not_implemented}.

-spec empty_table(atom()) -> ok | {aborted, term()}.
empty_table(_Table) ->
    {aborted, not_implemented}.

-spec list_table(atom()) -> [] | [#zone{}] | [#authorities{}] | [tuple()] |
                            {error, doesnt_exist} | {error, not_implemented}.
list_table(zones) ->
    {error, not_implemented};
list_table(authorities) ->
    {error, not_implemented};
list_table(_Name) ->
    {error, doesnt_exist}.

%%

named_ports_to_srv_records(Key, NamedPorts) ->
    maps:fold(fun(PortName, #{protocol := Protocol,
                              port := Port}, Acc) ->
                  [#dns_rr{name = <<"_", PortName/binary, "._", Protocol/binary, ".", Key/binary>>,
                           type = ?DNS_TYPE_SRV,
                           ttl = 3600,
                           data = #dns_rrdata_srv{port=Port,
                                                  target = Key,
                                                  priority=1,
                                                  weight=1}} | Acc]
              end, [], NamedPorts).

endpoints_to_a_records(Key, Endpoints) ->
    [#dns_rr{
        name = Key,
        type = ?DNS_TYPE_A,
        ttl = 3600,
        data = #dns_rrdata_a{ip = IP}
       } || #{ip := IP} <- Endpoints].

