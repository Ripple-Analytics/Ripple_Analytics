%%%-------------------------------------------------------------------
%%% @doc Science Detectors - Math, Physics, Engineering, Biology
%%% @end
%%%-------------------------------------------------------------------
-module(detectors_science).

-export([detect_mathematics/1, detect_physics/1, detect_engineering/1, detect_biology/1]).

detect_mathematics(Text) ->
    Keywords = [<<"equation">>, <<"formula">>, <<"calculation">>, <<"proof">>, 
                <<"theorem">>, <<"logic">>, <<"number">>, <<"variable">>, 
                <<"function">>, <<"algorithm">>, <<"probability">>, <<"statistics">>],
    MathSymbols = [<<"=">>, <<"+">>, <<"-">>, <<"*">>, <<"/">>, <<"sqrt">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    SymbolCount = detector_utils:count_keywords(Text, MathSymbols),
    NumberCount = detector_utils:count_numbers(Text),
    
    BaseScore = (KeywordCount * 0.05) + (SymbolCount * 0.1) + (min(NumberCount, 10) * 0.02),
    Score = min(1.0, BaseScore / 0.5),
    
    detector_utils:make_result(<<"Mathematics">>, <<"Hard Sciences">>, Score,
        #{<<"keywords">> => KeywordCount, <<"symbols">> => SymbolCount, <<"numbers">> => NumberCount},
        <<"mathematics">>).

detect_physics(Text) ->
    Keywords = [<<"force">>, <<"energy">>, <<"velocity">>, <<"acceleration">>, 
                <<"momentum">>, <<"gravity">>, <<"motion">>, <<"wave">>, 
                <<"particle">>, <<"quantum">>, <<"entropy">>, <<"pressure">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    Score = min(1.0, KeywordCount * 0.06),
    
    detector_utils:make_result(<<"Physics">>, <<"Hard Sciences">>, Score,
        #{<<"keywords">> => KeywordCount}, <<"physics">>).

detect_engineering(Text) ->
    Keywords = [<<"design">>, <<"system">>, <<"optimization">>, <<"efficiency">>, 
                <<"constraint">>, <<"trade-off">>, <<"specification">>, <<"tolerance">>,
                <<"failure">>, <<"redundancy">>, <<"feedback">>, <<"control">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    Score = min(1.0, KeywordCount * 0.05),
    
    detector_utils:make_result(<<"Engineering">>, <<"Hard Sciences">>, Score,
        #{<<"keywords">> => KeywordCount}, <<"engineering">>).

detect_biology(Text) ->
    Keywords = [<<"evolution">>, <<"natural selection">>, <<"adaptation">>, <<"ecosystem">>,
                <<"organism">>, <<"species">>, <<"mutation">>, <<"gene">>, <<"cell">>,
                <<"survival">>, <<"reproduction">>, <<"fitness">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    Score = min(1.0, KeywordCount * 0.06),
    
    detector_utils:make_result(<<"Biology">>, <<"Hard Sciences">>, Score,
        #{<<"keywords">> => KeywordCount}, <<"biology">>).
