-module(sdp_erldns_storage).

-include_lib("erldns/include/erldns.hrl").
-include_lib("kernel/include/logger.hrl").

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
insert(zones, #zone{} = Zone)->
    ?LOG_INFO("INSERT ~p", [Zone]),
    ok;
insert(zones, {_N, #zone{} = Zone})->
    ?LOG_INFO("INSERT ~p", [Zone]),
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
