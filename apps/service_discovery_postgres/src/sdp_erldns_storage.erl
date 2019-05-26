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
                    %% record_count = 0 :: non_neg_integer(),
                    records=Records,
                    %% records_by_name ::  #{binary() => [dns:rr()]} | trimmed,
                    keysets=Keysets})->
    A = [jsx:encode(erldns_zone_encoder:encode_record_json(Record)) || Record <- Authority],
    R = [jsx:encode(erldns_zone_encoder:encode_record_json(Record)) || Record <- Records],
    #{command := insert} =
        sdp_query:run(insert_zone, [Name, <<"1">>, {array, A}, {array, R}, {array, Keysets}]),
    ok;
insert(zones, {_N, #zone{} = Zone})->
    insert(zones, Zone),
    ok;
insert(authorities, #authorities{} = Auth) ->
    ?LOG_INFO("INSERT ~p", [Auth]),
    ok.

-spec delete_table(atom()) -> ok | {aborted, any()}.
delete_table(Table) ->
    ok.

-spec delete(Table :: atom(), Key :: term()) -> ok | any().
delete(Table, Key)->
    ok.

-spec backup_table(atom()) -> ok | {error, Reason :: term()}.
backup_table(_Table)->
    ok.

-spec backup_tables() -> ok | {error, Reason :: term()}.
backup_tables()->
    ok.

-spec select(Table :: atom(), Key :: term()) -> [tuple()].
select(zones, Key)->
    case sdp_query:run(select_zones, [Key]) of
        #{command := select, rows := []} ->
            [];
        #{command := select, rows := [Row | _]} ->
            {Name, Version, {array, _Authority}, {array, Records}, Keysets} = Row,
            DecodedRecords = [json_to_record(jsx:decode(R)) || {jsonb, R} <- Records],
            Authorities = lists:filter(erldns_records:match_type(?DNS_TYPE_SOA), DecodedRecords),
            [{Name, #zone{name=Name,
                          version=Version,
                          authority=Authorities,
                          record_count = length(DecodedRecords),
                          records=DecodedRecords,
                          records_by_name=erldns_zone_cache:build_named_index(DecodedRecords),
                          keysets=Keysets}}]
    end;
select(Table, Key)->
    ?LOG_INFO("SELECT ~p", [{Table, Key}]),
    [].

-spec select(atom(), list(), infinite | integer()) -> [tuple()].
select(_Table, MatchSpec, _Limit) ->
    ?LOG_INFO("SELECT ~p", [{_Table, MatchSpec}]),
    [].

-spec foldl(fun(), list(), atom())  -> Acc :: term() | {error, Reason :: term()}.
foldl(Iterator, _Acc, Table) ->
    ?LOG_INFO("FOLD ~p", [Table]),
    [].

-spec empty_table(atom()) -> ok | {aborted, term()}.
empty_table(_Table) ->
    ok.

-spec list_table(atom()) ->
  [] | [#zone{}] | [#authorities{}] | [tuple()] | {error, doesnt_exist}.
list_table(zones) ->
    ?LOG_INFO("LIST ZONES", []),
    [];
list_table(authorities) ->
    ?LOG_INFO("LIST AUTHORITIES", []),
    [];
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
