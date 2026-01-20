-module(recovery_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            Result = test_recovery(),
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

test_recovery() ->
    %% Test recovery by checking all services
    Services = [
        {<<"api_gateway">>, "http://api-gateway:8000/health"},
        {<<"analysis">>, "http://analysis-service:8001/health"},
        {<<"harvester">>, "http://harvester-service:8002/health"},
        {<<"storage">>, "http://storage-service:8003/health"}
    ],
    
    Results = lists:map(fun({Name, Url}) ->
        Status = case hackney:request(get, list_to_binary(Url), [], <<>>, [{timeout, 5000}]) of
            {ok, 200, _, _} -> <<"recovered">>;
            _ -> <<"not_recovered">>
        end,
        #{<<"service">> => Name, <<"status">> => Status}
    end, Services),
    
    AllRecovered = lists:all(fun(#{<<"status">> := S}) -> S =:= <<"recovered">> end, Results),
    
    #{
        <<"test">> => <<"recovery">>,
        <<"services">> => Results,
        <<"all_recovered">> => AllRecovered,
        <<"timestamp">> => erlang:system_time(second)
    }.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
