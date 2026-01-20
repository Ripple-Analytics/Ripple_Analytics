-module(deploy_build_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Data = jiffy:decode(Body, [return_maps]),
    CommitHash = binary_to_list(maps:get(<<"commit_hash">>, Data, <<"unknown">>)),
    spawn(fun() -> deployment_manager:build_standby(CommitHash) end),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jiffy:encode(#{status => <<"build_started">>, commit => list_to_binary(CommitHash)}),
        Req1),
    {ok, Req, State}.
