-module(sdh_conn).

-behaviour(acceptor).

-export([acceptor_init/3,
         acceptor_continue/3,
         acceptor_terminate/2]).

acceptor_init(_, _LSocket, {Transport, ElliCallback, ElliOpts, SslOpts}) ->
    {ok, {Transport, ElliCallback, ElliOpts, SslOpts}}.

acceptor_continue(_PeerName, Socket, {ssl, ElliCallback, ElliOpts, SslOpts}) ->
    {ok, AcceptSocket} = ssl:handshake(Socket, SslOpts),
    elli_http:keepalive_loop({ssl, AcceptSocket}, ElliOpts, ElliCallback);
acceptor_continue(_PeerName, Socket, {gen_tcp, ElliCallback, ElliOpts, _SslOpts}) ->
    elli_http:keepalive_loop({plain, Socket}, ElliOpts, ElliCallback).

acceptor_terminate(Reason, _) ->
    % Something went wrong. Either the acceptor_pool is terminating or the
    % accept failed.
    exit(Reason).
