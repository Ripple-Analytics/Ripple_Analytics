%%%-------------------------------------------------------------------
%%% @doc Google Drive Backup Application
%%% 
%%% Provides backup and restore functionality using Google Drive as
%%% a fallback source for the Mental Models System auto-updater.
%%% @end
%%%-------------------------------------------------------------------
-module(gdrive_backup_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Port = application:get_env(gdrive_backup, port, 8006),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/api/backup/status", backup_status_handler, []},
            {"/api/backup/create", backup_create_handler, []},
            {"/api/backup/restore", backup_restore_handler, []},
            {"/api/backup/list", backup_list_handler, []},
            {"/api/backup/config", backup_config_handler, []},
            {"/api/backup/download/:id", backup_download_handler, []},
            {"/[...]", not_found_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(http_listener, [{port, Port}],
        #{env => #{dispatch => Dispatch}}),
    
    io:format("Google Drive Backup Service started on port ~p~n", [Port]),
    gdrive_backup_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http_listener).
