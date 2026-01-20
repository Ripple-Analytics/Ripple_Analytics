-module(source_download_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Result = cache_worker:get_latest(),
    {Code, Body} = case Result of
        {ok, Data} -> {200, jsx:encode(Data)};
        {error, Err} -> {404, jsx:encode(#{error => Err})}
    end,
    Req = cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, Body, Req0),
    {ok, Req, State}.
