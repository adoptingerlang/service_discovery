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
    [load_sql(PrivDir, File) || File <- ["zones.sql", "services.sql"]].

load_sql(PrivDir, File) ->
    SqlFile = filename:join([PrivDir, "sql", File]),
    {ok, Queries} = eql:compile(SqlFile),
    io:format("Queries ~p ~p~n", [PrivDir, File]),
    [persistent_term:put({?MODULE, Name}, Query) || {Name, Query} <- Queries].
