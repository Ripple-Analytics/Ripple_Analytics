%%%-------------------------------------------------------------------
%%% @doc Business Detectors - Economics, Porter's, Moat, etc.
%%% @end
%%%-------------------------------------------------------------------
-module(detectors_business).

-export([detect_economics/1, detect_porters_five_forces/1, detect_economic_moat/1,
         detect_compound_interest/1, detect_network_effects/1, detect_margin_of_safety/1]).

detect_economics(Text) ->
    Keywords = [<<"supply">>, <<"demand">>, <<"market">>, <<"price">>,
                <<"cost">>, <<"value">>, <<"trade">>, <<"incentive">>,
                <<"scarcity">>, <<"opportunity cost">>, <<"marginal">>],
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    Score = min(1.0, KeywordCount * 0.05),
    detector_utils:make_result(<<"Economics">>, <<"Business">>, Score,
        #{<<"keywords">> => KeywordCount}, <<"economics">>).

detect_porters_five_forces(Text) ->
    Keywords = [<<"competition">>, <<"competitor">>, <<"supplier">>, <<"buyer">>,
                <<"substitute">>, <<"entry">>, <<"barrier">>, <<"threat">>,
                <<"bargaining">>, <<"power">>, <<"industry">>],
    Patterns = [<<"five forces">>, <<"competitive advantage">>, <<"market power">>],
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    PatternCount = detector_utils:count_patterns(TextLower, Patterns),
    Score = min(1.0, (KeywordCount * 0.04) + (PatternCount * 0.3)),
    detector_utils:make_result(<<"Porter's Five Forces">>, <<"Business">>, Score,
        #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount}, <<"porters_five_forces">>).

detect_economic_moat(Text) ->
    Keywords = [<<"moat">>, <<"advantage">>, <<"barrier">>, <<"protection">>,
                <<"brand">>, <<"patent">>, <<"switching cost">>, <<"scale">>],
    Patterns = [<<"competitive moat">>, <<"economic moat">>, <<"durable advantage">>],
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    PatternCount = detector_utils:count_patterns(TextLower, Patterns),
    Score = min(1.0, (KeywordCount * 0.04) + (PatternCount * 0.3)),
    detector_utils:make_result(<<"Economic Moat">>, <<"Business">>, Score,
        #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount}, <<"economic_moat">>).

detect_compound_interest(Text) ->
    Keywords = [<<"compound">>, <<"interest">>, <<"growth">>, <<"exponential">>,
                <<"accumulate">>, <<"reinvest">>, <<"snowball">>, <<"multiply">>],
    Patterns = [<<"compound interest">>, <<"compounding">>, <<"exponential growth">>],
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    PatternCount = detector_utils:count_patterns(TextLower, Patterns),
    Score = min(1.0, (KeywordCount * 0.04) + (PatternCount * 0.3)),
    detector_utils:make_result(<<"Compound Interest">>, <<"Business">>, Score,
        #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount}, <<"compound_interest">>).

detect_network_effects(Text) ->
    Keywords = [<<"network">>, <<"user">>, <<"platform">>, <<"viral">>,
                <<"adoption">>, <<"critical mass">>, <<"ecosystem">>],
    Patterns = [<<"network effect">>, <<"more users">>, <<"winner take all">>],
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    PatternCount = detector_utils:count_patterns(TextLower, Patterns),
    Score = min(1.0, (KeywordCount * 0.04) + (PatternCount * 0.3)),
    detector_utils:make_result(<<"Network Effects">>, <<"Business">>, Score,
        #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount}, <<"network_effects">>).

detect_margin_of_safety(Text) ->
    Keywords = [<<"margin">>, <<"safety">>, <<"buffer">>, <<"cushion">>,
                <<"conservative">>, <<"discount">>, <<"undervalue">>],
    Patterns = [<<"margin of safety">>, <<"safety margin">>, <<"room for error">>],
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    PatternCount = detector_utils:count_patterns(TextLower, Patterns),
    Score = min(1.0, (KeywordCount * 0.04) + (PatternCount * 0.3)),
    detector_utils:make_result(<<"Margin of Safety">>, <<"Business">>, Score,
        #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount}, <<"margin_of_safety">>).
