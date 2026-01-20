%%%-------------------------------------------------------------------
%%% @doc Thinking Detectors - Inversion, First Principles, etc.
%%% @end
%%%-------------------------------------------------------------------
-module(detectors_thinking).

-export([detect_inversion/1, detect_first_principles/1, detect_second_order_thinking/1]).

detect_inversion(Text) ->
    Keywords = [<<"opposite">>, <<"reverse">>, <<"avoid">>, <<"prevent">>,
                <<"instead">>, <<"rather">>, <<"negative">>, <<"failure">>],
    Patterns = [<<"what if">>, <<"how not to">>, <<"avoid failure">>,
                <<"invert">>, <<"think backwards">>],
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    PatternCount = detector_utils:count_patterns(TextLower, Patterns),
    Score = min(1.0, (KeywordCount * 0.04) + (PatternCount * 0.2)),
    detector_utils:make_result(<<"Inversion">>, <<"Thinking Tools">>, Score,
        #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount}, <<"inversion">>).

detect_first_principles(Text) ->
    Keywords = [<<"fundamental">>, <<"basic">>, <<"core">>, <<"essential">>,
                <<"underlying">>, <<"root">>, <<"foundation">>, <<"assumption">>],
    Patterns = [<<"first principles">>, <<"from scratch">>, <<"break down">>,
                <<"fundamental truth">>, <<"basic assumption">>],
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    PatternCount = detector_utils:count_patterns(TextLower, Patterns),
    Score = min(1.0, (KeywordCount * 0.04) + (PatternCount * 0.2)),
    detector_utils:make_result(<<"First Principles Thinking">>, <<"Thinking Tools">>, Score,
        #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount}, <<"first_principles">>).

detect_second_order_thinking(Text) ->
    Keywords = [<<"consequences">>, <<"effects">>, <<"long-term">>, <<"ripple">>,
                <<"cascade">>, <<"downstream">>, <<"implications">>],
    Patterns = [<<"second order">>, <<"and then">>, <<"what happens next">>,
                <<"long term">>, <<"downstream effect">>],
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    PatternCount = detector_utils:count_patterns(TextLower, Patterns),
    Score = min(1.0, (KeywordCount * 0.04) + (PatternCount * 0.2)),
    detector_utils:make_result(<<"Second-Order Thinking">>, <<"Thinking Tools">>, Score,
        #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount}, <<"second_order_thinking">>).
