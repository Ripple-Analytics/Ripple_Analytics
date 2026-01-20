-module(lan_not_found_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Response = jsx:encode(#{error => <<"Not found">>}),
    Req = cowboy_req:reply(404,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req0),
    {ok, Req, State}.
