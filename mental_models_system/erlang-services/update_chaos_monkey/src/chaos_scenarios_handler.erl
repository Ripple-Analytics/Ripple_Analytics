-module(chaos_scenarios_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    {ok, Scenarios} = chaos_worker:get_scenarios(),
    Body = jsx:encode(#{scenarios => Scenarios}),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req0),
    {ok, Req, State}.
