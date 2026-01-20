%%%-------------------------------------------------------------------
%%% @doc text_analyzer Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(text_analyzer_part2).

-export([detect_engineering/1, detect_biology/1, detect_psychology/1, detect_confirmation_bias/1, detect_anchoring_bias/1, detect_availability_heuristic/1]).

detect_engineering(Text) ->
    Keywords = [<<"design">>, <<"system">>, <<"optimization">>, <<"efficiency">>, 
                <<"constraint">>, <<"trade-off">>, <<"specification">>, <<"tolerance">>,
                <<"failure">>, <<"redundancy">>, <<"feedback">>, <<"control">>,
                <<"simulation">>, <<"prototype">>, <<"testing">>, <<"requirement">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    BaseScore = KeywordCount * 0.05,
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Engineering">>,
        <<"category">> => <<"Hard Sciences">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_biology(Text) ->
    Keywords = [<<"evolution">>, <<"natural selection">>, <<"adaptation">>, <<"ecosystem">>,
                <<"organism">>, <<"species">>, <<"mutation">>, <<"gene">>, <<"cell">>,
                <<"survival">>, <<"reproduction">>, <<"fitness">>, <<"niche">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    BaseScore = KeywordCount * 0.06,
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Biology">>,
        <<"category">> => <<"Hard Sciences">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount},
        <<"confidence">> => confidence_level(Score)
    }.

%%====================================================================
%% Detection Algorithms - Psychology & Biases
%%====================================================================

detect_psychology(Text) ->
    Keywords = [<<"behavior">>, <<"cognitive">>, <<"emotion">>, <<"motivation">>,
                <<"perception">>, <<"memory">>, <<"learning">>, <<"bias">>,
                <<"heuristic">>, <<"decision">>, <<"judgment">>, <<"thinking">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    BaseScore = KeywordCount * 0.05,
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Psychology">>,
        <<"category">> => <<"Psychology: Biases">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_confirmation_bias(Text) ->
    Keywords = [<<"confirm">>, <<"belief">>, <<"evidence">>, <<"support">>,
                <<"agree">>, <<"validate">>, <<"ignore">>, <<"contrary">>,
                <<"opposite">>, <<"proof">>, <<"convinced">>, <<"certain">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    %% Check for negative patterns
    PatternCount = count_patterns(TextLower, [
        <<"only look">>, <<"only see">>, <<"ignore evidence">>,
        <<"dismiss">>, <<"overlook">>, <<"always right">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Confirmation Bias">>,
        <<"category">> => <<"Psychology: Biases">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_anchoring_bias(Text) ->
    Keywords = [<<"first">>, <<"initial">>, <<"starting">>, <<"reference">>,
                <<"number">>, <<"price">>, <<"estimate">>, <<"adjust">>,
                <<"anchor">>, <<"fixed">>, <<"baseline">>, <<"comparison">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"first number">>, <<"starting point">>, <<"starting price">>,
        <<"anchored to">>, <<"compared to">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Anchoring">>,
        <<"category">> => <<"Psychology: Biases">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_availability_heuristic(Text) ->
    Keywords = [<<"recent">>, <<"memorable">>, <<"vivid">>, <<"salient">>,
                <<"common">>, <<"frequent">>, <<"recall">>, <<"remember">>,
                <<"example">>, <<"instance">>, <<"case">>, <<"heard">>,
                <<"seen">>, <<"read">>, <<"news">>, <<"media">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"recently heard">>, <<"just saw">>, <<"in the news">>,
        <<"everyone knows">>, <<"memorable example">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Availability Heuristic">>,
        <<"category">> => <<"Psychology: Biases">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

