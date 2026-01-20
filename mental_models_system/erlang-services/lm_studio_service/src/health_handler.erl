-module(health_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Response = jsx:encode(#{
        <<"status">> => <<"healthy">>,
        <<"service">> => <<"lm-studio-service">>,
        <<"timestamp">> => erlang:system_time(second)
    }),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response, Req0),
    {ok, Req, State}.
