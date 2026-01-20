%%%-------------------------------------------------------------------
%%% @doc Psychology Detectors - Biases and cognitive patterns
%%% @end
%%%-------------------------------------------------------------------
-module(detectors_psychology).

-export([detect_psychology/1, detect_confirmation_bias/1, detect_anchoring_bias/1,
         detect_availability_heuristic/1, detect_overconfidence_bias/1,
         detect_loss_aversion/1, detect_sunk_cost_fallacy/1]).

detect_psychology(Text) ->
    Keywords = [<<"behavior">>, <<"cognitive">>, <<"emotion">>, <<"motivation">>,
                <<"perception">>, <<"memory">>, <<"learning">>, <<"bias">>,
                <<"heuristic">>, <<"decision">>, <<"judgment">>, <<"thinking">>],
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    Score = min(1.0, KeywordCount * 0.05),
    detector_utils:make_result(<<"Psychology">>, <<"Psychology: Biases">>, Score,
        #{<<"keywords">> => KeywordCount}, <<"psychology">>).

detect_confirmation_bias(Text) ->
    Keywords = [<<"confirm">>, <<"belief">>, <<"evidence">>, <<"support">>,
                <<"agree">>, <<"validate">>, <<"ignore">>, <<"contrary">>],
    Patterns = [<<"only look">>, <<"only see">>, <<"ignore evidence">>, <<"dismiss">>],
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    PatternCount = detector_utils:count_patterns(TextLower, Patterns),
    Score = min(1.0, (KeywordCount * 0.04) + (PatternCount * 0.2)),
    detector_utils:make_result(<<"Confirmation Bias">>, <<"Psychology: Biases">>, Score,
        #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount}, <<"confirmation_bias">>).

detect_anchoring_bias(Text) ->
    Keywords = [<<"first">>, <<"initial">>, <<"starting">>, <<"reference">>,
                <<"number">>, <<"price">>, <<"estimate">>, <<"anchor">>],
    Patterns = [<<"first number">>, <<"starting point">>, <<"anchored to">>],
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    PatternCount = detector_utils:count_patterns(TextLower, Patterns),
    Score = min(1.0, (KeywordCount * 0.04) + (PatternCount * 0.2)),
    detector_utils:make_result(<<"Anchoring">>, <<"Psychology: Biases">>, Score,
        #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount}, <<"anchoring_bias">>).

detect_availability_heuristic(Text) ->
    Keywords = [<<"recent">>, <<"remember">>, <<"example">>, <<"instance">>,
                <<"recall">>, <<"vivid">>, <<"memorable">>, <<"salient">>],
    Patterns = [<<"comes to mind">>, <<"easy to remember">>, <<"recent example">>],
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    PatternCount = detector_utils:count_patterns(TextLower, Patterns),
    Score = min(1.0, (KeywordCount * 0.04) + (PatternCount * 0.2)),
    detector_utils:make_result(<<"Availability Heuristic">>, <<"Psychology: Biases">>, Score,
        #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount}, <<"availability_heuristic">>).

detect_overconfidence_bias(Text) ->
    Keywords = [<<"certain">>, <<"sure">>, <<"confident">>, <<"definitely">>,
                <<"absolutely">>, <<"guarantee">>, <<"impossible">>, <<"always">>],
    Patterns = [<<"100%">>, <<"no doubt">>, <<"for sure">>, <<"without question">>],
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    PatternCount = detector_utils:count_patterns(TextLower, Patterns),
    Score = min(1.0, (KeywordCount * 0.04) + (PatternCount * 0.2)),
    detector_utils:make_result(<<"Overconfidence Bias">>, <<"Psychology: Biases">>, Score,
        #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount}, <<"overconfidence_bias">>).

detect_loss_aversion(Text) ->
    Keywords = [<<"lose">>, <<"loss">>, <<"risk">>, <<"avoid">>,
                <<"protect">>, <<"safe">>, <<"fear">>, <<"downside">>],
    Patterns = [<<"afraid to lose">>, <<"can't afford">>, <<"too risky">>],
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    PatternCount = detector_utils:count_patterns(TextLower, Patterns),
    Score = min(1.0, (KeywordCount * 0.04) + (PatternCount * 0.2)),
    detector_utils:make_result(<<"Loss Aversion">>, <<"Psychology: Biases">>, Score,
        #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount}, <<"loss_aversion">>).

detect_sunk_cost_fallacy(Text) ->
    Keywords = [<<"invested">>, <<"spent">>, <<"already">>, <<"committed">>,
                <<"wasted">>, <<"effort">>, <<"time">>, <<"money">>],
    Patterns = [<<"too much invested">>, <<"can't quit now">>, <<"already spent">>],
    TextLower = string:lowercase(Text),
    KeywordCount = detector_utils:count_keywords(TextLower, Keywords),
    PatternCount = detector_utils:count_patterns(TextLower, Patterns),
    Score = min(1.0, (KeywordCount * 0.04) + (PatternCount * 0.2)),
    detector_utils:make_result(<<"Sunk Cost Fallacy">>, <<"Psychology: Biases">>, Score,
        #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount}, <<"sunk_cost_fallacy">>).
