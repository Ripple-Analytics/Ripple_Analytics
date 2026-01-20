%%%-------------------------------------------------------------------
%%% @doc Batch Scrape Handler - Handles batch URL scraping requests
%%%-------------------------------------------------------------------
-module(batch_scrape_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            handle_batch_scrape(Body, Req1, State);
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_batch_scrape(Body, Req0, State) ->
    try jsx:decode(Body, [return_maps]) of
        Params ->
            Urls = maps:get(<<"urls">>, Params, []),
            case Urls of
                [] ->
                    Req1 = cowboy_req:reply(400, cors_headers(),
                        jsx:encode(#{<<"error">> => <<"No URLs provided">>}), Req0),
                    {ok, Req1, State};
                _ when length(Urls) > 50 ->
                    Req1 = cowboy_req:reply(400, cors_headers(),
                        jsx:encode(#{<<"error">> => <<"Maximum 50 URLs per batch">>}), Req0),
                    {ok, Req1, State};
                _ ->
                    %% Process URLs in parallel
                    Results = scrape_urls_parallel(Urls),
                    stats_collector:record_batch_scrape(length(Urls), length([R || {ok, _} = R <- Results])),
                    
                    Response = jsx:encode(#{
                        <<"success">> => true,
                        <<"total">> => length(Urls),
                        <<"successful">> => length([R || {ok, _} = R <- Results]),
                        <<"failed">> => length([R || {error, _} = R <- Results]),
                        <<"results">> => format_results(Results)
                    }),
                    Req2 = cowboy_req:reply(200, cors_headers(), Response, Req0),
                    {ok, Req2, State}
            end
    catch
        _:_ ->
            Req3 = cowboy_req:reply(400, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Invalid JSON">>}), Req0),
            {ok, Req3, State}
    end.

scrape_urls_parallel(Urls) ->
    Parent = self(),
    Refs = lists:map(fun(Url) ->
        Ref = make_ref(),
        spawn(fun() ->
            Result = scraper_pool:scrape_url(Url),
            Parent ! {Ref, Url, Result}
        end),
        {Ref, Url}
    end, Urls),
    
    %% Collect results with timeout
    collect_results(Refs, [], 30000).

collect_results([], Acc, _Timeout) ->
    lists:reverse(Acc);
collect_results([{Ref, Url} | Rest], Acc, Timeout) ->
    receive
        {Ref, Url, Result} ->
            collect_results(Rest, [{Url, Result} | Acc], Timeout)
    after Timeout ->
        %% Timeout for this URL
        collect_results(Rest, [{Url, {error, timeout}} | Acc], Timeout)
    end.

format_results(Results) ->
    lists:map(fun({Url, Result}) ->
        case Result of
            {ok, Data} ->
                #{
                    <<"url">> => Url,
                    <<"success">> => true,
                    <<"content_type">> => maps:get(<<"content_type">>, Data, <<"unknown">>),
                    <<"size">> => maps:get(<<"size">>, Data, 0)
                };
            {error, Reason} ->
                #{
                    <<"url">> => Url,
                    <<"success">> => false,
                    <<"error">> => format_error(Reason)
                }
        end
    end, Results).

format_error(timeout) -> <<"Request timed out">>;
format_error(#{<<"status">> := Status}) -> 
    <<"HTTP ", (integer_to_binary(Status))/binary>>;
format_error(#{<<"reason">> := Reason}) -> Reason;
format_error(_) -> <<"Unknown error">>.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
