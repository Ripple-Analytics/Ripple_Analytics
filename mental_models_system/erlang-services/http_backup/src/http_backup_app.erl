-module(http_backup_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", http_health_handler, []},
            {"/api/http/status", http_status_handler, []},
            {"/api/http/config", http_config_handler, []},
            {"/api/http/sources", http_sources_handler, []},
            {"/api/http/download", http_download_handler, []},
            {"/api/http/test", http_test_handler, []},
            {'_', http_not_found_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 8009}],
        #{env => #{dispatch => Dispatch}}
    ),
    io:format("HTTP/HTTPS Backup Service started on port 8009~n"),
    http_backup_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http_listener),
    ok.
