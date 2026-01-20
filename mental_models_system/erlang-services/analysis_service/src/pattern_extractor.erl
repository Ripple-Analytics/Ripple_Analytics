%%%-------------------------------------------------------------------
%%% @doc Pattern Extractor
%%% 
%%% Extracts recurring patterns, themes, and insights from text.
%%% Identifies decision patterns, thinking patterns, and common
%%% cognitive structures.
%%% @end
%%%-------------------------------------------------------------------
-module(pattern_extractor).
%% Helper modules: pattern_extractor_part2, pattern_extractor_part3, pattern_extractor_part4

-export([extract_patterns/1, extract_key_insights/1, generate_inverted_perspective/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc Extract recurring patterns from text
extract_patterns(Text) when is_binary(Text) ->
    LowerText = string:lowercase(binary_to_list(Text)),
    
    %% Extract different types of patterns
    DecisionPatterns = extract_decision_patterns(LowerText),
    ThinkingPatterns = extract_thinking_patterns(LowerText),
    EmotionalPatterns = extract_emotional_patterns(LowerText),
    TemporalPatterns = extract_temporal_patterns(LowerText),
    
    AllPatterns = DecisionPatterns ++ ThinkingPatterns ++ EmotionalPatterns ++ TemporalPatterns,
    
    %% Sort by confidence
    Sorted = lists:sort(fun(A, B) ->
        maps:get(<<"confidence">>, A, 0) >= maps:get(<<"confidence">>, B, 0)
    end, AllPatterns),
    
    #{
        <<"success">> => true,
        <<"patterns">> => Sorted,
        <<"pattern_count">> => length(Sorted),
        <<"categories">> => #{
            <<"decision">> => length(DecisionPatterns),
            <<"thinking">> => length(ThinkingPatterns),
            <<"emotional">> => length(EmotionalPatterns),
            <<"temporal">> => length(TemporalPatterns)
        }
    };

extract_patterns(_) ->
    #{<<"success">> => false, <<"error">> => <<"Invalid input">>}.

%% @doc Extract key insights from text
extract_key_insights(Text) when is_binary(Text) ->
    LowerText = string:lowercase(binary_to_list(Text)),
    
    Insights = [],
    
    %% Check for contrast patterns (but, however, although)
    I1 = case has_contrast_pattern(LowerText) of
        true -> [#{
            <<"type">> => <<"contrast">>,
            <<"insight">> => <<"Text contains contrasting viewpoints - consider both sides">>,
            <<"action">> => <<"Identify the tension between opposing ideas and find synthesis">>
        } | Insights];
        false -> Insights
    end,
    
    %% Check for causal patterns (because, therefore, thus)
    I2 = case has_causal_pattern(LowerText) of
        true -> [#{
            <<"type">> => <<"causal">>,
            <<"insight">> => <<"Causal reasoning detected - verify the cause-effect relationship">>,
            <<"action">> => <<"Question whether correlation implies causation">>
        } | I1];
        false -> I1
    end,
    
    %% Check for uncertainty patterns (maybe, might, could)
    I3 = case has_uncertainty_pattern(LowerText) of
        true -> [#{
            <<"type">> => <<"uncertainty">>,
            <<"insight">> => <<"Uncertainty expressed - consider probability ranges">>,
            <<"action">> => <<"Assign confidence levels to uncertain claims">>
        } | I2];
        false -> I2
    end,
    
    %% Check for absolute patterns (always, never, everyone)
    I4 = case has_absolute_pattern(LowerText) of
        true -> [#{
            <<"type">> => <<"absolute">>,
            <<"insight">> => <<"Absolute language detected - likely oversimplification">>,
            <<"action">> => <<"Look for exceptions and edge cases">>
        } | I3];
        false -> I3
    end,
    
    %% Check for comparison patterns (better, worse, more, less)
    I5 = case has_comparison_pattern(LowerText) of
        true -> [#{
            <<"type">> => <<"comparison">>,
            <<"insight">> => <<"Comparative analysis present - ensure fair comparison">>,
            <<"action">> => <<"Verify comparison criteria are appropriate and complete">>
        } | I4];
        false -> I4
    end,
    
    #{
        <<"success">> => true,
        <<"insights">> => I5,
        <<"insight_count">> => length(I5)
    };

extract_key_insights(_) ->
    #{<<"success">> => false, <<"error">> => <<"Invalid input">>}.

%% @doc Generate inverted perspective (Inversion mental model)