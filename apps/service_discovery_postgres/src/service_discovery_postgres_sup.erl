%%%-------------------------------------------------------------------
%% @doc service_discovery top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(service_discovery_postgres_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    PoolConfig = #{host => "localhost",
                   port => 5432,
                   user => "discovery",
                   password => "password",
                   database => "discovery",

                   %% pool specific settings
                   %% pool_size => integer(),
                   %% queue_target => integer(),
                   %% queue_interval => integer(),
                   %% idle_interval => integer(),
                   queue => true,
                   trace => false,
                   decode_opts => []},

    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 10},
    ChildSpec = #{id => sdp_pool,
                  start => {pgo_pool, start_link, [default, PoolConfig]}},
    {ok, {SupFlags, [ChildSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
