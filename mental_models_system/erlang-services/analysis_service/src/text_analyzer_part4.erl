%%%-------------------------------------------------------------------
%%% @doc text_analyzer Helper Module - Part 4
%%% @end
%%%-------------------------------------------------------------------
-module(text_analyzer_part4).

-export([detect_margin_of_safety/1, detect_compound_interest/1, detect_network_effects/1, detect_inversion/1, detect_first_principles/1]).

detect_margin_of_safety(Text) ->
    Keywords = [<<"safety">>, <<"buffer">>, <<"margin">>, <<"conservative">>,
                <<"cushion">>, <<"protection">>, <<"downside">>, <<"risk">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"margin of safety">>, <<"safety buffer">>, <<"conservative estimate">>,
        <<"downside protection">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Margin of Safety">>,
        <<"category">> => <<"Economics & Finance">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_compound_interest(Text) ->
    Keywords = [<<"compound">>, <<"exponential">>, <<"growth">>, <<"time">>,
                <<"accumulate">>, <<"interest">>, <<"reinvest">>, <<"snowball">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"compound interest">>, <<"exponential growth">>, <<"over time">>,
        <<"snowball effect">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Compound Interest">>,
        <<"category">> => <<"Economics & Finance">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_network_effects(Text) ->
    Keywords = [<<"network">>, <<"effect">>, <<"users">>, <<"platform">>,
                <<"viral">>, <<"adoption">>, <<"critical mass">>, <<"ecosystem">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"network effect">>, <<"more users">>, <<"platform">>,
        <<"viral growth">>, <<"critical mass">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Network Effects">>,
        <<"category">> => <<"Economics & Finance">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

%%====================================================================
%% Detection Algorithms - Thinking Tools
%%====================================================================

detect_inversion(Text) ->
    Keywords = [<<"opposite">>, <<"reverse">>, <<"invert">>, <<"backward">>,
                <<"contrary">>, <<"avoid">>, <<"prevent">>, <<"instead">>,
                <<"flip">>, <<"inverse">>, <<"don't">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"instead of">>, <<"rather than">>, <<"opposite of">>,
        <<"how not to">>, <<"how to avoid">>, <<"inversion">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Inversion">>,
        <<"category">> => <<"Thinking Tools">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_first_principles(Text) ->
    Keywords = [<<"fundamental">>, <<"basic">>, <<"truth">>, <<"reasoning">>,
                <<"axiom">>, <<"assumption">>, <<"ground up">>, <<"scratch">>,
                <<"core">>, <<"essence">>, <<"underlying">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"first principles">>, <<"from scratch">>, <<"ground up">>,
        <<"fundamental truth">>, <<"basic assumption">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"First Principles Thinking">>,
        <<"category">> => <<"Thinking Tools">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

