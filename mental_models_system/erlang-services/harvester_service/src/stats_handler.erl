%%%-------------------------------------------------------------------
%%% @doc Stats Handler - Returns harvesting statistics
%%%-------------------------------------------------------------------
-module(stats_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Stats = stats_collector:get_stats(),
    Response = jsx:encode(Stats),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>},
        Response, Req0),
    {ok, Req, State}.
