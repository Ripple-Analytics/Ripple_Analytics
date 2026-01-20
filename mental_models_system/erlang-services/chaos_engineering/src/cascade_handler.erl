-module(cascade_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            Result = run_cascade_scenario(),
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

run_cascade_scenario() ->
    %% Simulate cascade failure scenario
    Steps = [
        #{<<"step">> => 1, <<"action">> => <<"inject_latency">>, <<"target">> => <<"storage">>},
        #{<<"step">> => 2, <<"action">> => <<"check_propagation">>, <<"target">> => <<"harvester">>},
        #{<<"step">> => 3, <<"action">> => <<"check_propagation">>, <<"target">> => <<"analysis">>},
        #{<<"step">> => 4, <<"action">> => <<"check_gateway">>, <<"target">> => <<"api_gateway">>}
    ],
    
    %% Execute steps and collect results
    Results = lists:map(fun(#{<<"step">> := N, <<"action">> := Action, <<"target">> := Target}) ->
        timer:sleep(500),  %% Wait between steps
        Status = execute_cascade_step(Action, Target),
        #{<<"step">> => N, <<"action">> => Action, <<"target">> => Target, <<"result">> => Status}
    end, Steps),
    
    #{
        <<"scenario">> => <<"cascade_failure">>,
        <<"steps">> => Results,
        <<"completed">> => true,
        <<"timestamp">> => erlang:system_time(second)
    }.

execute_cascade_step(<<"inject_latency">>, _Target) ->
    chaos_controller:inject_latency(<<"storage">>, 2000),
    <<"latency_injected">>;
execute_cascade_step(<<"check_propagation">>, Target) ->
    Url = get_health_url(Target),
    case hackney:request(get, Url, [], <<>>, [{timeout, 5000}]) of
        {ok, 200, _, _} -> <<"healthy">>;
        _ -> <<"affected">>
    end;
execute_cascade_step(<<"check_gateway">>, _Target) ->
    case hackney:request(get, <<"http://api-gateway:8000/health">>, [], <<>>, [{timeout, 5000}]) of
        {ok, 200, _, _} -> <<"healthy">>;
        _ -> <<"affected">>
    end.

get_health_url(<<"harvester">>) -> <<"http://harvester-service:8002/health">>;
get_health_url(<<"analysis">>) -> <<"http://analysis-service:8001/health">>;
get_health_url(<<"storage">>) -> <<"http://storage-service:8003/health">>;
get_health_url(_) -> <<"http://api-gateway:8000/health">>.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
