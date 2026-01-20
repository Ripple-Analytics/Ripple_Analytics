-module(source_check_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    {ok, Result} = gdrive_worker:check_available(),
    Body = jsx:encode(Result),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req0),
    {ok, Req, State}.
