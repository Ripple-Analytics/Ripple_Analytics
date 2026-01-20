%%%-------------------------------------------------------------------
%%% @doc Google Drive Source Service
%%% Provides updates from Google Drive backup.
%%% Also syncs latest code to Google Drive for backup.
%%% @end
%%%-------------------------------------------------------------------
-module(gdrive_source_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("[GDRIVE-SOURCE] Starting Google Drive Source Service~n"),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/api/source/status", source_status_handler, []},
            {"/api/source/check", source_check_handler, []},
            {"/api/source/fetch", source_fetch_handler, []},
            {"/api/source/download", source_download_handler, []},
            {"/api/backup/upload", backup_upload_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, 8011}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("[GDRIVE-SOURCE] HTTP API listening on port 8011~n"),
    gdrive_source_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http_listener),
    ok.
