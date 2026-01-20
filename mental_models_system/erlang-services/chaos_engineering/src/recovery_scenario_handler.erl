-module(recovery_scenario_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            Result = run_recovery_scenario(),
            Req = cowboy_req:reply(200, cors_headers(), jsx:encode(Result), Req0),
            {ok, Req, State};
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

run_recovery_scenario() ->
    %% Full recovery test scenario
    Steps = [
        #{<<"step">> => 1, <<"action">> => <<"baseline_check">>},
        #{<<"step">> => 2, <<"action">> => <<"inject_failures">>},
        #{<<"step">> => 3, <<"action">> => <<"verify_degradation">>},
        #{<<"step">> => 4, <<"action">> => <<"clear_failures">>},
        #{<<"step">> => 5, <<"action">> => <<"verify_recovery">>}
    ],
    
    Results = lists:map(fun(#{<<"step">> := N, <<"action">> := Action}) ->
        timer:sleep(1000),
        Status = execute_recovery_step(Action),
        #{<<"step">> => N, <<"action">> => Action, <<"result">> => Status}
    end, Steps),
    
    FinalHealth = chaos_controller:get_service_health(),
    AllHealthy = lists:all(fun(#{status := S}) -> S =:= <<"healthy">> end, FinalHealth),
    
    #{
        <<"scenario">> => <<"recovery_test">>,
        <<"steps">> => Results,
        <<"final_health">> => FinalHealth,
        <<"recovery_successful">> => AllHealthy,
        <<"timestamp">> => erlang:system_time(second)
    }.

execute_recovery_step(<<"baseline_check">>) ->
    Health = chaos_controller:get_service_health(),
    Healthy = length([S || #{status := S} <- Health, S =:= <<"healthy">>]),
    #{<<"healthy_services">> => Healthy, <<"total">> => length(Health)};

execute_recovery_step(<<"inject_failures">>) ->
    chaos_controller:inject_latency(<<"analysis">>, 3000),
    chaos_controller:inject_error(<<"harvester">>, 0.5),
    <<"failures_injected">>;

execute_recovery_step(<<"verify_degradation">>) ->
    Health = chaos_controller:get_service_health(),
    Degraded = length([S || #{status := S} <- Health, S =/= <<"healthy">>]),
    #{<<"degraded_services">> => Degraded};

execute_recovery_step(<<"clear_failures">>) ->
    chaos_controller:clear_injections(),
    <<"failures_cleared">>;

execute_recovery_step(<<"verify_recovery">>) ->
    timer:sleep(2000),  %% Wait for recovery
    Health = chaos_controller:get_service_health(),
    Healthy = length([S || #{status := S} <- Health, S =:= <<"healthy">>]),
    #{<<"recovered_services">> => Healthy}.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
