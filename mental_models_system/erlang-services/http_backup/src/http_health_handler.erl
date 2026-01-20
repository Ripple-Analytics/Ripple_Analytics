-module(http_health_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Response = jsx:encode(#{status => <<"healthy">>, service => <<"http-backup">>}),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req0),
    {ok, Req, State}.
