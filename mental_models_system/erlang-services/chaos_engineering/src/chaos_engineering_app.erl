%%%-------------------------------------------------------------------
%%% @doc Chaos Engineering Service - Resilience Testing with OTP
%%% 
%%% Provides fault injection, circuit breaker testing, load testing,
%%% and recovery validation for the Mental Models System.
%%% @end
%%%-------------------------------------------------------------------
-module(chaos_engineering_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Port = application:get_env(chaos_engineering, port, 8005),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/services/health", services_health_handler, []},
            {"/inject/latency", latency_handler, []},
            {"/inject/error", error_handler, []},
            {"/inject/failure", failure_handler, []},
            {"/test/circuit-breaker", circuit_breaker_handler, []},
            {"/test/load", load_test_handler, []},
            {"/test/recovery", recovery_handler, []},
            {"/scenario/cascade-failure", cascade_handler, []},
            {"/scenario/recovery-test", recovery_scenario_handler, []},
            {"/[...]", not_found_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(http_listener, [{port, Port}],
        #{env => #{dispatch => Dispatch}}),
    
    io:format("Chaos Engineering Service started on port ~p~n", [Port]),
    chaos_engineering_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http_listener).
