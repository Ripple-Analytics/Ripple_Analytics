%%%-------------------------------------------------------------------
%%% @doc Scrape Handler - Handles URL scraping requests
%%%-------------------------------------------------------------------
-module(scrape_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            handle_scrape(Body, Req1, State);
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_scrape(Body, Req0, State) ->
    try jsx:decode(Body, [return_maps]) of
        Params ->
            Url = maps:get(<<"url">>, Params, <<>>),
            case Url of
                <<>> ->
                    Req1 = cowboy_req:reply(400, cors_headers(),
                        jsx:encode(#{<<"error">> => <<"No URL provided">>}), Req0),
                    {ok, Req1, State};
                _ ->
                    Result = scraper_pool:scrape_url(Url),
                    stats_collector:record_scrape(Result),
                    Response = case Result of
                        {ok, Data} -> jsx:encode(maps:put(<<"success">>, true, Data));
                        {error, Error} -> jsx:encode(maps:put(<<"success">>, false, Error))
                    end,
                    Req2 = cowboy_req:reply(200, cors_headers(), Response, Req0),
                    {ok, Req2, State}
            end
    catch
        _:_ ->
            Req3 = cowboy_req:reply(400, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Invalid JSON">>}), Req0),
            {ok, Req3, State}
    end.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
