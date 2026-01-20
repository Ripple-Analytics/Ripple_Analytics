%%%-------------------------------------------------------------------
%%% @doc Process Handler - Handles file processing requests
%%%-------------------------------------------------------------------
-module(process_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            handle_process(Body, Req1, State);
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_process(Body, Req0, State) ->
    try
        Params = jsx:decode(Body, [return_maps]),
        Type = maps:get(<<"type">>, Params, <<"file">>),
        Content = maps:get(<<"content">>, Params, <<>>),
        
        Result = scraper_pool:process_file(Type, Content),
        stats_collector:record_process(Result),
        Response = case Result of
            {ok, Data} -> jsx:encode(maps:put(<<"success">>, true, Data));
            {error, Error} -> jsx:encode(maps:put(<<"success">>, false, Error))
        end,
        Req = cowboy_req:reply(200, cors_headers(), Response, Req0),
        {ok, Req, State}
    catch
        _:_ ->
            Req = cowboy_req:reply(400, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Invalid JSON">>}), Req0),
            {ok, Req, State}
    end.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
