-module(source_download_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    {ok, Path} = github_worker:get_download_path(),
    Body = jsx:encode(#{path => list_to_binary(Path), source => <<"github">>}),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req0),
    {ok, Req, State}.
