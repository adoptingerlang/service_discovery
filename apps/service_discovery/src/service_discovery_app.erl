%%%-------------------------------------------------------------------
%% @doc service_discovery public API
%% @end
%%%-------------------------------------------------------------------

-module(service_discovery_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    io:format("HELLO AGAIN~n"),
    {ok, StorageMod} = application:get_env(service_discovery, storage_module),
    sds_storage:configure_storage(StorageMod),
    service_discovery_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
