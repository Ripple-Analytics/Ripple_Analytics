%%%-------------------------------------------------------------------
%%% @doc Local Cache Service
%%% Maintains a local cache of the last known good version.
%%% Acts as ultimate fallback when all remote sources fail.
%%% @end
%%%-------------------------------------------------------------------
-module(local_cache_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("[LOCAL-CACHE] Starting Local Cache Service~n"),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/api/source/status", source_status_handler, []},
            {"/api/source/download", source_download_handler, []},
            {"/api/cache/save", cache_save_handler, []},
            {"/api/cache/versions", cache_versions_handler, []},
            {"/api/cache/rollback/:version", cache_rollback_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, 8012}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("[LOCAL-CACHE] HTTP API listening on port 8012~n"),
    local_cache_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http_listener),
    ok.
