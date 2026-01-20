%%%-------------------------------------------------------------------
%%% @doc Update Chaos Monkey Service
%%% Actively tests the resilience of the update system by:
%%% - Simulating source failures
%%% - Testing fallback chains
%%% - Verifying recovery mechanisms
%%% - Stress testing update operations
%%% @end
%%%-------------------------------------------------------------------
-module(chaos_monkey_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("[CHAOS-MONKEY] Starting Update Chaos Monkey Service~n"),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/api/chaos/status", chaos_status_handler, []},
            {"/api/chaos/run", chaos_run_handler, []},
            {"/api/chaos/scenarios", chaos_scenarios_handler, []},
            {"/api/chaos/results", chaos_results_handler, []},
            {"/api/chaos/config", chaos_config_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, 8013}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("[CHAOS-MONKEY] HTTP API listening on port 8013~n"),
    chaos_monkey_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http_listener),
    ok.
