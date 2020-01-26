%%%-------------------------------------------------------------------
%% @doc service_discovery top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(service_discovery_http_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(DEFAULT_ELLI_OPTS, #{callback => {sdh_handler, []},
                             accept_timeout => 10000,
                             request_timeout => 60000,
                             header_timeout => 10000,
                             body_timeout => 30000,
                             max_body_size => 1024000}).
-define(DEFAULT_LISTEN_OPTS, #{port => 3000,
                               socket_opts => [{reuseaddr, true},
                                               {nodelay, true},
                                               {reuseaddr, true},
                                               {backlog, 32768},
                                               {keepalive, true}]}).
-define(DEFAULT_ACCEPTOR_OPTS, #{pool_size => 10}).

start_link(Opts) ->
    ElliOpts = proplists:get_value(elli_opts, Opts, #{}),
    ListenOpts = proplists:get_value(listen_opts, Opts, #{}),
    AcceptorOpts = proplists:get_value(acceptor_opts, Opts, #{}),
    start_link(ElliOpts, ListenOpts, AcceptorOpts).

start_link(ElliOpts, ListenOpts, AcceptorOpts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [maps:merge(?DEFAULT_ELLI_OPTS, ElliOpts),
                                                      maps:merge(?DEFAULT_LISTEN_OPTS, ListenOpts),
                                                      maps:merge(?DEFAULT_ACCEPTOR_OPTS, AcceptorOpts)]).

init([ElliOpts, ListenOpts, AcceptorOpts]) ->
    RestartStrategy = #{strategy => rest_for_one,
                        intensity => 5,
                        period => 10},
    Pool = #{id => sdh_pool,
             start => {sdh_pool, start_link, [ElliOpts, ListenOpts]},
             restart => permanent,
             shutdown => 5000,
             type => worker,
             modules => [sdh_handler]},
    Socket = #{id => sdh_socket,
               start => {sdh_socket, start_link, [ListenOpts, AcceptorOpts]},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [sdh_socket]},

    %% NOTE: The order is important here. Socket is terminated first when the supervisor stops.
    %% This means the listen socket will be closed before the pool of acceptors will shutdown.
    %% We want the listen socket to stop listening before we start shutting down acceptors.
    {ok, {RestartStrategy, [Pool, Socket]}}.
