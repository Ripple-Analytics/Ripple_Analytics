%%%-------------------------------------------------------------------
%%% @doc Chaos Monkey Application
%%% 
%%% Proactively tests system resilience by randomly:
%%% - Killing containers
%%% - Injecting network latency
%%% - Simulating service failures
%%% - Testing recovery mechanisms
%%% @end
%%%-------------------------------------------------------------------
-module(chaos_monkey_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Port = application:get_env(chaos_monkey, port, 8007),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", cm_health_handler, []},
            {"/api/chaos/status", cm_status_handler, []},
            {"/api/chaos/enable", cm_enable_handler, []},
            {"/api/chaos/disable", cm_disable_handler, []},
            {"/api/chaos/attack", cm_attack_handler, []},
            {"/api/chaos/attacks", cm_attacks_handler, []},
            {"/api/chaos/config", cm_config_handler, []},
            {"/api/chaos/history", cm_history_handler, []},
            {"/[...]", cm_not_found_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(http_listener, [{port, Port}],
        #{env => #{dispatch => Dispatch}}),
    
    io:format("Chaos Monkey started on port ~p~n", [Port]),
    chaos_monkey_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http_listener).
