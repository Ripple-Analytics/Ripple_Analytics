-module(health_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(200, cors_headers(),
        jsx:encode(#{<<"status">> => <<"healthy">>, <<"service">> => <<"chaos-engineering">>}), Req0),
    {ok, Req, State}.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>}.
