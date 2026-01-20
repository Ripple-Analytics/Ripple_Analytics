%%%-------------------------------------------------------------------
%%% @doc Report Generation HTTP Handler
%%% 
%%% REST API for generating analysis reports.
%%% 
%%% Endpoints:
%%% POST /api/analysis/report - Generate HTML report from analysis data
%%% @end
%%%-------------------------------------------------------------------
-module(report_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Req = handle_request(Method, Req0),
    {ok, Req, State}.

handle_request(<<"POST">>, Req0) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        #{<<"action">> := <<"generate">>} = Params ->
            handle_generate(Params, Req);
        #{<<"action">> := <<"analyze_and_report">>} = Params ->
            handle_analyze_and_report(Params, Req);
        _ ->
            json_response(400, #{
                <<"error">> => <<"Missing or invalid action">>,
                <<"valid_actions">> => [<<"generate">>, <<"analyze_and_report">>]
            }, Req)
    end;

handle_request(<<"GET">>, Req) ->
    json_response(200, #{
        <<"service">> => <<"report_generator">>,
        <<"actions">> => [
            #{<<"name">> => <<"generate">>, <<"description">> => <<"Generate HTML report from analysis result">>},
            #{<<"name">> => <<"analyze_and_report">>, <<"description">> => <<"Analyze text and generate report">>}
        ]
    }, Req);

handle_request(_, Req) ->
    json_response(405, #{<<"error">> => <<"Method not allowed">>}, Req).

handle_generate(Params, Req) ->
    AnalysisResult = maps:get(<<"result">>, Params, #{}),
    Title = maps:get(<<"title">>, Params, <<"Mental Models Analysis Report">>),
    
    Options = #{title => Title},
    Html = report_generator:generate_html(AnalysisResult, Options),
    
    case maps:get(<<"format">>, Params, <<"html">>) of
        <<"html">> ->
            html_response(Html, Req);
        <<"json">> ->
            json_response(200, #{
                <<"success">> => true,
                <<"html">> => Html,
                <<"size">> => byte_size(Html)
            }, Req)
    end.

handle_analyze_and_report(Params, Req) ->
    Text = maps:get(<<"text">>, Params, <<>>),
    Title = maps:get(<<"title">>, Params, <<"Mental Models Analysis Report">>),
    
    case Text of
        <<>> ->
            json_response(400, #{<<"error">> => <<"Text is required">>}, Req);
        _ ->
            TextAnalysis = text_analyzer:analyze_text(Text),
            Models = maps:get(<<"models">>, TextAnalysis, []),
            TopModels = maps:get(<<"top_models">>, TextAnalysis, []),
            
            HighScoring = [M || M <- Models, maps:get(<<"score">>, M, 0) >= 70],
            LollapaloozaDetected = length(HighScoring) >= 3,
            
            Patterns = pattern_extractor:extract_patterns(Text),
            PatternList = maps:get(<<"patterns">>, Patterns, []),
            
            Insights = pattern_extractor:extract_key_insights(Text),
            InsightList = maps:get(<<"insights">>, Insights, []),
            
            AnalysisResult = #{
                <<"success">> => true,
                <<"models">> => TopModels,
                <<"all_models">> => Models,
                <<"high_scoring_count">> => length(HighScoring),
                <<"lollapalooza_detected">> => LollapaloozaDetected,
                <<"patterns">> => PatternList,
                <<"insights">> => InsightList
            },
            
            Options = #{title => Title},
            Html = report_generator:generate_html(AnalysisResult, Options),
            
            case maps:get(<<"format">>, Params, <<"html">>) of
                <<"html">> ->
                    html_response(Html, Req);
                <<"json">> ->
                    json_response(200, #{
                        <<"success">> => true,
                        <<"html">> => Html,
                        <<"analysis">> => AnalysisResult,
                        <<"size">> => byte_size(Html)
                    }, Req)
            end
    end.

html_response(Html, Req) ->
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>,
        <<"content-disposition">> => <<"attachment; filename=\"analysis_report.html\"">>
    }, Html, Req).

json_response(Status, Body, Req) ->
    cowboy_req:reply(Status, #{
        <<"content-type">> => <<"application/json">>,
        <<"access-control-allow-origin">> => <<"*">>
    }, jsx:encode(Body), Req).
