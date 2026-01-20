%%%-------------------------------------------------------------------
%%% @doc Google Drive Backup Application
%%% Provides automatic backup to Google Drive and serves as a fallback
%%% source for the auto-updater when GitHub is unavailable.
%%%
%%% Features:
%%% - Automatic periodic backups to Google Drive
%%% - Manual backup trigger via REST API
%%% - Download endpoint for auto-updater fallback
%%% - Backup verification and integrity checks
%%% @end
%%%-------------------------------------------------------------------
-module(gdrive_backup_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("[GDRIVE-BACKUP] Starting Google Drive Backup Service~n"),
    
    %% Start the HTTP API
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/api/backup/status", backup_status_handler, []},
            {"/api/backup/create", backup_create_handler, []},
            {"/api/backup/list", backup_list_handler, []},
            {"/api/backup/download/:filename", backup_download_handler, []},
            {"/api/backup/verify", backup_verify_handler, []},
            {"/api/backup/config", backup_config_handler, []},
            {"/api/backup/test", backup_test_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, 8007}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("[GDRIVE-BACKUP] HTTP API listening on port 8007~n"),
    
    %% Start the supervisor
    gdrive_backup_sup:start_link().

stop(_State) ->
    io:format("[GDRIVE-BACKUP] Stopping Google Drive Backup Service~n"),
    ok = cowboy:stop_listener(http_listener),
    ok.
