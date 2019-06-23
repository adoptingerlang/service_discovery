%%%-------------------------------------------------------------------
%% @doc service_discovery top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(service_discovery_http_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    ElliOpts = [{callback, sdh_handler},
                {port, 3000}],
    ElliSpec = #{id => sdh_handler,
                 start => {elli, start_link, [ElliOpts]},
                 restart => permanent,
                 shutdown => 5000,
                 type => worker,
                 modules => [sdh_handler]},
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 10},
    {ok, {SupFlags, [ElliSpec]}}.
