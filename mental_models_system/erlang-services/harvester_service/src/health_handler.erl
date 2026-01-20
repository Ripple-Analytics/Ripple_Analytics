%%%-------------------------------------------------------------------
%%% @doc Health Handler
%%%-------------------------------------------------------------------
-module(health_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Response = jsx:encode(#{
        <<"status">> => <<"healthy">>,
        <<"service">> => <<"harvester-service">>,
        <<"version">> => <<"1.0.0">>
    }),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>},
        Response, Req0),
    {ok, Req, State}.
