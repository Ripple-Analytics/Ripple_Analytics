%%%-------------------------------------------------------------------
%%% @doc Updater Config Handler
%%% GET /api/updater/config - Returns current configuration
%%% @end
%%%-------------------------------------------------------------------
-module(updater_config_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    {ok, Config} = updater_worker:get_config(),
    Response = jsx:encode(Config),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>},
        Response,
        Req0),
    {ok, Req, State}.
