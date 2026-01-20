-module(s3_status_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Status = s3_worker:get_status(),
    Response = jsx:encode(#{success => true, s3 => Status}),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req0),
    {ok, Req, State}.
