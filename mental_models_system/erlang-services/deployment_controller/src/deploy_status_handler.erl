-module(deploy_status_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    {ok, Status} = deployment_manager:get_status(),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jiffy:encode(Status),
        Req0),
    {ok, Req, State}.
