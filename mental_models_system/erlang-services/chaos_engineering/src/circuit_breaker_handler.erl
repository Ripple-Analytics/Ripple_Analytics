-module(circuit_breaker_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            try jsx:decode(Body, [return_maps]) of
                Params ->
                    Service = maps:get(<<"service">>, Params, <<"api_gateway">>),
                    Result = test_circuit_breaker(Service),
                    Req2 = cowboy_req:reply(200, cors_headers(), jsx:encode(Result), Req1),
                    {ok, Req2, State}
            catch _:_ ->
                Req3 = cowboy_req:reply(400, cors_headers(),
                    jsx:encode(#{<<"error">> => <<"Invalid request">>}), Req1),
                {ok, Req3, State}
            end;
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

test_circuit_breaker(Service) ->
    %% Simulate circuit breaker test by making rapid requests
    Results = lists:map(fun(_) ->
        timer:sleep(100),
        case hackney:request(get, <<"http://api-gateway:8000/health">>, [], <<>>, [{timeout, 5000}]) of
            {ok, 200, _, _} -> success;
            _ -> failure
        end
    end, lists:seq(1, 10)),
    
    Successes = length([R || R <- Results, R =:= success]),
    Failures = length([R || R <- Results, R =:= failure]),
    
    #{
        <<"service">> => Service,
        <<"test">> => <<"circuit_breaker">>,
        <<"requests">> => 10,
        <<"successes">> => Successes,
        <<"failures">> => Failures,
        <<"circuit_status">> => if Failures > 5 -> <<"open">>; true -> <<"closed">> end
    }.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
