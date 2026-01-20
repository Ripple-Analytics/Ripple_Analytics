-module(cache_save_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    {ok, ReqBody, Req1} = cowboy_req:read_body(Req0),
    Data = jsx:decode(ReqBody, [return_maps]),
    SourcePath = maps:get(<<"source_path">>, Data, <<"/repo/github">>),
    Commit = maps:get(<<"commit">>, Data, <<"unknown">>),
    Result = cache_worker:save_version(binary_to_list(SourcePath), Commit),
    {Code, Body} = case Result of
        {ok, Msg} -> {200, jsx:encode(Msg)};
        {error, Err} -> {500, jsx:encode(#{error => Err})}
    end,
    Req = cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, Body, Req1),
    {ok, Req, State}.
