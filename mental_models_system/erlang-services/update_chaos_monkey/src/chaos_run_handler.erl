-module(chaos_run_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    {ok, ReqBody, Req1} = cowboy_req:read_body(Req0),
    Data = jsx:decode(ReqBody, [return_maps]),
    ScenarioId = maps:get(<<"scenario">>, Data, <<"test_github_source">>),
    Result = chaos_worker:run_scenario(ScenarioId),
    {Code, Body} = case Result of
        {ok, R} -> {200, jsx:encode(R)};
        {error, Err} -> {500, jsx:encode(#{error => Err})}
    end,
    Req = cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, Body, Req1),
    {ok, Req, State}.
