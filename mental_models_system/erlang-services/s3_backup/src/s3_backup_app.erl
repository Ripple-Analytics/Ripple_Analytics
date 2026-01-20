-module(s3_backup_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", s3_health_handler, []},
            {"/api/s3/status", s3_status_handler, []},
            {"/api/s3/config", s3_config_handler, []},
            {"/api/s3/backup/create", s3_backup_handler, []},
            {"/api/s3/backup/list", s3_list_handler, []},
            {"/api/s3/backup/restore", s3_restore_handler, []},
            {"/api/s3/test", s3_test_handler, []},
            {'_', s3_not_found_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(s3_http_listener,
        [{port, 8008}],
        #{env => #{dispatch => Dispatch}}
    ),
    io:format("S3/MinIO Backup Service started on port 8008~n"),
    s3_backup_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(s3_http_listener),
    ok.
