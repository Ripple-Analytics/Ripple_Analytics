%%%-------------------------------------------------------------------
%%% @doc Updater Status Handler
%%% GET /api/updater/status - Returns current updater status
%%% @end
%%%-------------------------------------------------------------------
-module(updater_status_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    {ok, Status} = updater_worker:get_status(),
    Response = jsx:encode(Status),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>},
        Response,
        Req0),
    {ok, Req, State}.
