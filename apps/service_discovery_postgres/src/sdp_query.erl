-module(sdp_query).

-export([get/1,
         get/2,
         run/2,
         load_sql/0]).

get(Name) ->
    persistent_term:get({?MODULE, Name}).

get(Name, Params) ->
    lists:map(fun(Key) when is_atom(Key) ->
                      proplists:get_value(Key, Params);
                 (S) ->
                      S
              end, ?MODULE:get(Name)).

run(QueryName, Args) ->
    pgo:query(?MODULE:get(QueryName), Args).

load_sql() ->
    PrivDir = code:priv_dir(service_discovery_postgres),
    SqlFile = filename:join([PrivDir, "sql", "zones.sql"]),
    {ok, Queries} = eql:compile(SqlFile),
    [persistent_term:put({?MODULE, Name}, Query) || {Name, Query} <- Queries].
