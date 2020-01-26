-module(sdh_socket).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%% public api

start_link(ListenOpts, AcceptorOpts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ListenOpts, AcceptorOpts], []).

%% gen_server api

init([ListenOpts, AcceptorOpts]) ->
    Port = maps:get(port, ListenOpts, 3000),
    IPAddress = maps:get(ip, ListenOpts, {0,0,0,0}),
    AcceptorPoolSize = maps:get(pool_size, AcceptorOpts, 10),
    SocketOpts = maps:get(socket_opts, ListenOpts, [{reuseaddr, true},
                                                    {nodelay, true},
                                                    {reuseaddr, true},
                                                    {backlog, 32768},
                                                    {keepalive, true}]),
    % Trapping exit so can close socket in terminate/2
    _ = process_flag(trap_exit, true),
    Opts = [{active, false}, {mode, binary}, {packet, raw}, {ip, IPAddress} | SocketOpts],
    case gen_tcp:listen(Port, Opts) of
        {ok, Socket} ->
            MRef = monitor(port, Socket),
            sdh_pool:accept_socket(Socket, AcceptorPoolSize),
            {ok, {Socket, MRef}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(Req, _, State) ->
    {stop, {bad_call, Req}, State}.

handle_cast(Req, State) ->
    {stop, {bad_cast, Req}, State}.

handle_info({'DOWN', MRef, port, Socket, Reason}, {Socket, MRef} = State) ->
    {stop, Reason, State};
handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, {Socket, MRef}) ->
    % Socket may already be down but need to ensure it is closed to avoid
    % eaddrinuse error on restart
    case demonitor(MRef, [flush, info]) of
        true  -> gen_tcp:close(Socket);
        false -> ok
    end.
