-module(sdh_pool).

-behaviour(acceptor_pool).

-export([start_link/2,
         accept_socket/2]).

-export([init/1]).

start_link(ElliOpts, ListenOpts) ->
    acceptor_pool:start_link({local, ?MODULE}, ?MODULE, [ElliOpts, ListenOpts]).

accept_socket(Socket, Acceptors) ->
    acceptor_pool:accept_socket(?MODULE, Socket, Acceptors).

%% TODO: add ssl support
init([ElliOpts, ListenOpts]) ->
    ElliCallback = maps:get(callback, ElliOpts),

    %% Grace gives a 5 second shutdown period for open connections
    Conn = #{id => sdh_conn,
             start => {sdh_conn, {gen_tcp, ElliCallback, maps:to_list(ElliOpts), ListenOpts}, []},
             grace => timer:seconds(5)},
    {ok, {#{}, [Conn]}}.
