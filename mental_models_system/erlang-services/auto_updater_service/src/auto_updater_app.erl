%%%-------------------------------------------------------------------
%%% @doc Auto-Updater Application
%%% Bulletproof update system with multiple fallback sources:
%%% 1. SSH (private repo with deploy key)
%%% 2. HTTPS with PAT (authenticated)
%%% 3. HTTPS public (if repo is public)
%%% 4. Google Drive backup
%%%
%%% Provides REST API for UI integration and automatic background updates.
%%% @end
%%%-------------------------------------------------------------------
-module(auto_updater_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("[AUTO-UPDATER] Starting Auto-Updater Service~n"),
    
    %% Start the HTTP API
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/api/updater/status", updater_status_handler, []},
            {"/api/updater/check", updater_check_handler, []},
            {"/api/updater/update", updater_update_handler, []},
            {"/api/updater/config", updater_config_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, 8006}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("[AUTO-UPDATER] HTTP API listening on port 8006~n"),
    
    %% Start the supervisor
    auto_updater_sup:start_link().

stop(_State) ->
    io:format("[AUTO-UPDATER] Stopping Auto-Updater Service~n"),
    ok = cowboy:stop_listener(http_listener),
    ok.
