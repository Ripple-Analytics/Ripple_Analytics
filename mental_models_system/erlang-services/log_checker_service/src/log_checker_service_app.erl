%%%-------------------------------------------------------------------
%%% @doc Log Checker Service Application
%%% Blue-green log monitoring with automated failure detection and LM Studio fix pipeline.
%%% Monitors all service logs, detects errors, and automatically triggers fixes.
%%% @end
%%%-------------------------------------------------------------------
-module(log_checker_service_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("[LOG-CHECKER] ========================================~n"),
    io:format("[LOG-CHECKER] AUTOMATED LOG CHECKER STARTING~n"),
    io:format("[LOG-CHECKER] Monitoring all services for failures~n"),
    io:format("[LOG-CHECKER] Auto-fix pipeline: ENABLED~n"),
    io:format("[LOG-CHECKER] ========================================~n"),
    
    Port = list_to_integer(os:getenv("PORT", "8040")),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/api/logs/status", log_status_handler, []},
            {"/api/logs/errors", log_errors_handler, []},
            {"/api/logs/trigger-scan", log_scan_handler, []},
            {"/api/logs/fix-history", fix_history_handler, []},
            {"/api/logs/services", services_handler, []},
            {'_', not_found_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("[LOG-CHECKER] HTTP server started on port ~p~n", [Port]),
    
    log_checker_service_sup:start_link().

stop(_State) ->
    ok.
