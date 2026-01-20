%%%-------------------------------------------------------------------
%%% @doc Deployment Controller Application
%%% Blue-green deployment orchestrator with self-update capability.
%%% @end
%%%-------------------------------------------------------------------
-module(deployment_controller_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("[DEPLOY-CTRL] ========================================~n"),
    io:format("[DEPLOY-CTRL] DEPLOYMENT CONTROLLER STARTING~n"),
    io:format("[DEPLOY-CTRL] Environment: ~s~n", [get_env()]),
    io:format("[DEPLOY-CTRL] ========================================~n"),
    
    Port = list_to_integer(os:getenv("PORT", "8007")),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/api/deploy/status", deploy_status_handler, []},
            {"/api/deploy/build", deploy_build_handler, []},
            {"/api/deploy/switch", deploy_switch_handler, []},
            {"/api/deploy/rollback", deploy_rollback_handler, []},
            {"/api/deploy/self-update", self_update_handler, []},
            {"/api/deploy/peer-handoff", peer_handoff_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("[DEPLOY-CTRL] HTTP server started on port ~p~n", [Port]),
    
    deployment_controller_sup:start_link().

stop(_State) ->
    ok.

get_env() ->
    os:getenv("DEPLOYMENT_ENV", "blue").
