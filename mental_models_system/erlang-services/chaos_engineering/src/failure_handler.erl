-module(failure_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            try jsx:decode(Body, [return_maps]) of
                Params ->
                    Service = maps:get(<<"service">>, Params),
                    {ok, Result} = chaos_controller:inject_failure(Service),
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

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
