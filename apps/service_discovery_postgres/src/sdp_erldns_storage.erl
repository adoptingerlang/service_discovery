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

-spec create(atom()) -> ok.
create(schema) ->
    ok;
create(zones) ->
    ok;
create(authorities) ->
    ok.

-spec insert(atom(), any()) -> any().
insert(zones, #zone{name=Name,
                    version=_Version,
                    authority=Authority,
                    records=Records,
                    keysets=Keysets})->
    %% A = [jsx:encode(erldns_zone_encoder:encode_record_json(Record)) || Record <- Authority],
    %% R = [jsx:encode(erldns_zone_encoder:encode_record_json(Record)) || Record <- Records],
    %% #{command := insert} =
    %%     sdp_query:run(insert_zone, [Name, <<"1">>, {array, A}, {array, R}, {array, Keysets}]),
    ok;
insert(zones, {_N, #zone{} = Zone})->
    insert(zones, Zone);
insert(authorities, #authorities{} = _Auth) ->
    {error, not_implemented}.

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
    io:format("Key ~p~n", [Key]),
    case sdp_services_storage:read_endpoints(Key) of
        [] ->
            [];
        Endpoints ->
            io:format("Endpoints ~p~n", [Endpoints]),
            Version = <<>>,
            Authorities = [#dns_rr{name = Key,
                                   type = ?DNS_TYPE_SOA,
                                   ttl = 3600,
                                   data = #dns_rrdata_soa{mname = <<"ns1.", Key/binary>>,
                                                          rname = <<"admin.", Key/binary>>,
                                                          serial = 2013022001,
                                                          refresh = 86400,
                                                          retry = 7200,
                                                          expire = 604800,
                                                          minimum = 300}}],
            SrvRecord = #dns_rr{
                           name = <<"_http._tcp.", Key/binary>>,
                           type = ?DNS_TYPE_SRV,
                           ttl = 3600,
                           data = #dns_rrdata_srv{port=8080,
                                                  target = Key,
                                                  priority=1,
                                                  weight=1}},
            Records = [SrvRecord | [#dns_rr{
                          name = Key,
                          type = ?DNS_TYPE_A,
                          ttl = 3600,
                          data = #dns_rrdata_a{ip = IP}
                         } || #{ip := IP} <- Endpoints]],
            [{Key, #zone{name=Key,
                         version=Version,
                         authority=Authorities,
                         record_count = length(Records),
                         records=Records,
                         records_by_name=erldns_zone_cache:build_named_index(Records),
                         keysets=[]}}]
    end;

    %% case sdp_query:run(select_zones, [Key]) of
    %%     #{command := select, rows := []} ->
    %%         [];
    %%     #{command := select, rows := [Row | _]} ->
    %%         {Name, Version, {array, _Authority}, {array, Records}, Keysets} = Row,
    %%         DecodedRecords = [json_to_record(jsx:decode(R)) || {jsonb, R} <- Records],
    %%         Authorities = lists:filter(erldns_records:match_type(?DNS_TYPE_SOA), DecodedRecords),
    %%         [{Name, #zone{name=Name,
    %%                       version=Version,
    %%                       authority=Authorities,
    %%                       record_count = length(DecodedRecords),
    %%                       records=DecodedRecords,
    %%                       records_by_name=erldns_zone_cache:build_named_index(DecodedRecords),
    %%                       keysets=Keysets}}]
    %% end;
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

json_to_record(Record) ->
    List = json_record_to_list(Record),
    erldns_zone_parser:json_record_to_erlang(List).

json_record_to_list(JsonRecord) ->
  [
   erldns_config:keyget(<<"name">>, JsonRecord),
   erldns_config:keyget(<<"type">>, JsonRecord),
   erldns_config:keyget(<<"ttl">>, JsonRecord),
   erldns_config:keyget(<<"content">>, JsonRecord),
   erldns_config:keyget(<<"context">>, JsonRecord)
  ].
