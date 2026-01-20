-module(blue_green_orchestrator_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/api/deployment/status", deployment_status_handler, []},
            {"/api/deployment/switch", deployment_switch_handler, []},
            {"/api/deployment/switch/:service", deployment_switch_handler, []},
            {"/api/deployment/rollback/:service", deployment_rollback_handler, []},
            {"/api/deployment/services", deployment_services_handler, []},
            {"/api/deployment/health/:service", deployment_health_handler, []},
            {"/api/nginx/config", nginx_config_handler, []},
            {"/[...]", proxy_handler, []}
        ]}
    ]),
    
    Port = list_to_integer(os:getenv("LISTEN_PORT", "8020")),
    
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("[ORCHESTRATOR] Blue-Green Deployment Orchestrator started on port ~p~n", [Port]),
    io:format("[ORCHESTRATOR] Deployment environment: ~s~n", [os:getenv("DEPLOYMENT_ENV", "unknown")]),
    
    blue_green_orchestrator_sup:start_link().

stop(_State) ->
    ok.
