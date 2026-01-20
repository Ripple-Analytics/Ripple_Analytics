%%%-------------------------------------------------------------------
%%% @doc GitHub Source Service
%%% Provides updates from GitHub via SSH, PAT, or public HTTPS.
%%% Exposes REST API for the orchestrator to query.
%%% @end
%%%-------------------------------------------------------------------
-module(github_source_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("[GITHUB-SOURCE] Starting GitHub Source Service~n"),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/api/source/status", source_status_handler, []},
            {"/api/source/check", source_check_handler, []},
            {"/api/source/fetch", source_fetch_handler, []},
            {"/api/source/download", source_download_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, 8010}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("[GITHUB-SOURCE] HTTP API listening on port 8010~n"),
    github_source_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http_listener),
    ok.
