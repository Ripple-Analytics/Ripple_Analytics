-module(health_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>},
        jsx:encode(#{<<"status">> => <<"healthy">>, <<"service">> => <<"desktop-ui">>}), Req0),
    {ok, Req, State}.
