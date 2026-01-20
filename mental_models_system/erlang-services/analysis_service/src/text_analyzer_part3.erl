%%%-------------------------------------------------------------------
%%% @doc text_analyzer Helper Module - Part 3
%%% @end
%%%-------------------------------------------------------------------
-module(text_analyzer_part3).

-export([detect_overconfidence_bias/1, detect_loss_aversion/1, detect_sunk_cost_fallacy/1, detect_economics/1, detect_porters_five_forces/1, detect_economic_moat/1]).

detect_overconfidence_bias(Text) ->
    Keywords = [<<"certain">>, <<"sure">>, <<"confident">>, <<"know">>,
                <<"definitely">>, <<"absolutely">>, <<"guarantee">>,
                <<"impossible">>, <<"always">>, <<"never">>, <<"proven">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"absolutely will">>, <<"definitely will">>, <<"certainly will">>,
        <<"impossible to">>, <<"guaranteed to">>, <<"always happen">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Overconfidence">>,
        <<"category">> => <<"Psychology: Biases">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_loss_aversion(Text) ->
    Keywords = [<<"loss">>, <<"lose">>, <<"losing">>, <<"lost">>,
                <<"risk">>, <<"downside">>, <<"protect">>, <<"preserve">>,
                <<"avoid">>, <<"fear">>, <<"worry">>, <<"safe">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"fear of losing">>, <<"avoid loss">>, <<"protect from">>,
        <<"can't afford to lose">>, <<"too risky">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Loss Aversion">>,
        <<"category">> => <<"Psychology: Biases">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_sunk_cost_fallacy(Text) ->
    Keywords = [<<"invested">>, <<"spent">>, <<"already">>, <<"committed">>,
                <<"too much">>, <<"can't stop">>, <<"come this far">>,
                <<"wasted">>, <<"effort">>, <<"time">>, <<"money">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"already invested">>, <<"too much time">>, <<"can't give up now">>,
        <<"come too far">>, <<"wasted effort">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Sunk Cost Fallacy">>,
        <<"category">> => <<"Psychology: Biases">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

%%====================================================================
%% Detection Algorithms - Economics & Business
%%====================================================================

detect_economics(Text) ->
    Keywords = [<<"market">>, <<"price">>, <<"supply">>, <<"demand">>,
                <<"cost">>, <<"profit">>, <<"value">>, <<"trade">>,
                <<"incentive">>, <<"competition">>, <<"monopoly">>, <<"scarcity">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    BaseScore = KeywordCount * 0.05,
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Economics">>,
        <<"category">> => <<"Economics & Finance">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_porters_five_forces(Text) ->
    Keywords = [<<"supplier">>, <<"buyer">>, <<"competitor">>, <<"threat">>,
                <<"bargaining power">>, <<"substitute">>, <<"entry">>,
                <<"exit">>, <<"industry">>, <<"competitive">>, <<"rivalry">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"five forces">>, <<"supplier power">>, <<"buyer power">>,
        <<"threat of">>, <<"competitive advantage">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Porter's Five Forces">>,
        <<"category">> => <<"Economics & Finance">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_economic_moat(Text) ->
    Keywords = [<<"moat">>, <<"defensible">>, <<"competitive advantage">>,
                <<"barrier">>, <<"protection">>, <<"brand">>, <<"network">>,
                <<"switching cost">>, <<"scale">>, <<"cost advantage">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"economic moat">>, <<"competitive barrier">>, <<"switching cost">>,
        <<"network effect">>, <<"brand loyalty">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Economic Moat">>,
        <<"category">> => <<"Economics & Finance">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

