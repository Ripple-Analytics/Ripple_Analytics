%%%-------------------------------------------------------------------
%%% @doc Scrape and Analyze Handler - Scrape URL and analyze for mental models
%%%-------------------------------------------------------------------
-module(scrape_analyze_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            handle_scrape_analyze(Body, Req1, State);
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_scrape_analyze(Body, Req0, State) ->
    try jsx:decode(Body, [return_maps]) of
        Params ->
            Url = maps:get(<<"url">>, Params, <<>>),
            TopN = maps:get(<<"top_n">>, Params, 5),
            DetectBiases = maps:get(<<"detect_biases">>, Params, true),
            
            case Url of
                <<>> ->
                    Req1 = cowboy_req:reply(400, cors_headers(),
                        jsx:encode(#{<<"error">> => <<"No URL provided">>}), Req0),
                    {ok, Req1, State};
                _ ->
                    %% Step 1: Scrape the URL
                    ScrapeResult = scraper_pool:scrape_url(Url),
                    stats_collector:record_scrape(ScrapeResult),
                    
                    case ScrapeResult of
                        {ok, #{<<"content">> := Content} = ScrapeData} ->
                            %% Step 2: Extract text from HTML
                            TextContent = extract_text(Content),
                            
                            %% Step 3: Analyze for mental models
                            AnalysisResult = analyze_text(TextContent, TopN, DetectBiases),
                            
                            Response = jsx:encode(#{
                                <<"success">> => true,
                                <<"url">> => Url,
                                <<"scrape">> => #{
                                    <<"content_type">> => maps:get(<<"content_type">>, ScrapeData, <<"text/html">>),
                                    <<"size">> => maps:get(<<"size">>, ScrapeData, 0),
                                    <<"text_length">> => byte_size(TextContent)
                                },
                                <<"analysis">> => AnalysisResult
                            }),
                            Req2 = cowboy_req:reply(200, cors_headers(), Response, Req0),
                            {ok, Req2, State};
                        {error, Error} ->
                            Response = jsx:encode(#{
                                <<"success">> => false,
                                <<"url">> => Url,
                                <<"error">> => format_error(Error)
                            }),
                            Req2 = cowboy_req:reply(200, cors_headers(), Response, Req0),
                            {ok, Req2, State}
                    end
            end
    catch
        _:_ ->
            Req3 = cowboy_req:reply(400, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Invalid JSON">>}), Req0),
            {ok, Req3, State}
    end.

extract_text(Content) when is_binary(Content) ->
    %% Remove script and style tags first
    NoScript = re:replace(Content, <<"<script[^>]*>.*?</script>">>, <<" ">>, 
        [global, {return, binary}, caseless, dotall]),
    NoStyle = re:replace(NoScript, <<"<style[^>]*>.*?</style>">>, <<" ">>, 
        [global, {return, binary}, caseless, dotall]),
    %% Remove all HTML tags
    NoTags = re:replace(NoStyle, <<"<[^>]*>">>, <<" ">>, [global, {return, binary}]),
    %% Decode HTML entities
    Decoded = decode_entities(NoTags),
    %% Normalize whitespace
    re:replace(Decoded, <<"\\s+">>, <<" ">>, [global, {return, binary}]).

decode_entities(Text) ->
    T1 = binary:replace(Text, <<"&nbsp;">>, <<" ">>, [global]),
    T2 = binary:replace(T1, <<"&amp;">>, <<"&">>, [global]),
    T3 = binary:replace(T2, <<"&lt;">>, <<"<">>, [global]),
    T4 = binary:replace(T3, <<"&gt;">>, <<">">>, [global]),
    T5 = binary:replace(T4, <<"&quot;">>, <<"\"">>, [global]),
    binary:replace(T5, <<"&#39;">>, <<"'">>, [global]).

analyze_text(Text, TopN, DetectBiases) ->
    %% Call the Analysis Service API
    AnalysisUrl = "http://analysis-service:8001/api/analysis/analyze",
    BiasUrl = "http://analysis-service:8001/api/analysis/detect-biases",
    
    Headers = [{<<"content-type">>, <<"application/json">>}],
    
    %% Get mental models
    ModelsBody = jsx:encode(#{<<"text">> => Text, <<"top_n">> => TopN}),
    ModelsResult = case hackney:request(post, list_to_binary(AnalysisUrl), Headers, ModelsBody, [{timeout, 30000}]) of
        {ok, 200, _, ModelsRef} ->
            {ok, ModelsRespBody} = hackney:body(ModelsRef),
            jsx:decode(ModelsRespBody, [return_maps]);
        _ ->
            #{<<"models">> => [], <<"error">> => <<"Analysis service unavailable">>}
    end,
    
    %% Get biases if requested
    BiasesResult = case DetectBiases of
        true ->
            BiasBody = jsx:encode(#{<<"text">> => Text}),
            case hackney:request(post, list_to_binary(BiasUrl), Headers, BiasBody, [{timeout, 30000}]) of
                {ok, 200, _, BiasRef} ->
                    {ok, BiasRespBody} = hackney:body(BiasRef),
                    jsx:decode(BiasRespBody, [return_maps]);
                _ ->
                    #{<<"biases">> => []}
            end;
        false ->
            #{<<"biases">> => []}
    end,
    
    #{
        <<"models">> => maps:get(<<"models">>, ModelsResult, []),
        <<"biases">> => maps:get(<<"biases">>, BiasesResult, []),
        <<"method">> => maps:get(<<"method">>, ModelsResult, <<"unknown">>)
    }.

format_error(#{<<"status">> := Status}) -> 
    <<"HTTP error: ", (integer_to_binary(Status))/binary>>;
format_error(#{<<"reason">> := Reason}) -> Reason;
format_error(_) -> <<"Unknown error">>.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
