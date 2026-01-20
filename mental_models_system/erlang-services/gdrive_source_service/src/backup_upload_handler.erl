-module(backup_upload_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),
    SourcePath = maps:get(<<"source_path">>, Data, <<"/repo/github">>),
    Result = gdrive_worker:upload_backup(binary_to_list(SourcePath)),
    {Code, RespBody} = case Result of
        {ok, Msg} -> {200, jsx:encode(Msg)};
        {error, Err} -> {500, jsx:encode(#{error => Err})}
    end,
    Req = cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, RespBody, Req1),
    {ok, Req, State}.
