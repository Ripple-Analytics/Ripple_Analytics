-module(health_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jiffy:encode(#{status => <<"healthy">>, service => <<"deployment-controller">>}),
        Req0),
    {ok, Req, State}.
