%%%-------------------------------------------------------------------
%%% @doc Pattern Extractor
%%% 
%%% Extracts recurring patterns, themes, and insights from text.
%%% Identifies decision patterns, thinking patterns, and common
%%% cognitive structures.
%%% @end
%%%-------------------------------------------------------------------
-module(pattern_extractor).

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
generate_inverted_perspective(Text) when is_binary(Text) ->
    LowerText = string:lowercase(binary_to_list(Text)),
    
    Inversions = [],
    
    %% Invert success to failure
    Inv1 = case string:find(LowerText, "success") =/= nomatch orelse
                string:find(LowerText, "achieve") =/= nomatch orelse
                string:find(LowerText, "goal") =/= nomatch of
        true -> [#{
            <<"original">> => <<"Focus on achieving success">>,
            <<"inverted">> => <<"Focus on avoiding failure">>,
            <<"question">> => <<"What would guarantee failure? Avoid those things.">>
        } | Inversions];
        false -> Inversions
    end,
    
    %% Invert growth to decline
    Inv2 = case string:find(LowerText, "grow") =/= nomatch orelse
                string:find(LowerText, "increase") =/= nomatch orelse
                string:find(LowerText, "expand") =/= nomatch of
        true -> [#{
            <<"original">> => <<"How to grow/increase">>,
            <<"inverted">> => <<"How to prevent decline/decrease">>,
            <<"question">> => <<"What causes decline? Eliminate those factors.">>
        } | Inv1];
        false -> Inv1
    end,
    
    %% Invert customer acquisition to retention
    Inv3 = case string:find(LowerText, "customer") =/= nomatch orelse
                string:find(LowerText, "user") =/= nomatch orelse
                string:find(LowerText, "client") =/= nomatch of
        true -> [#{
            <<"original">> => <<"How to acquire customers">>,
            <<"inverted">> => <<"How to not lose customers">>,
            <<"question">> => <<"What makes customers leave? Fix those issues first.">>
        } | Inv2];
        false -> Inv2
    end,
    
    %% Invert profit to loss
    Inv4 = case string:find(LowerText, "profit") =/= nomatch orelse
                string:find(LowerText, "revenue") =/= nomatch orelse
                string:find(LowerText, "money") =/= nomatch of
        true -> [#{
            <<"original">> => <<"How to make money">>,
            <<"inverted">> => <<"How to not lose money">>,
            <<"question">> => <<"What destroys value? Avoid those activities.">>
        } | Inv3];
        false -> Inv3
    end,
    
    %% Invert efficiency to waste
    Inv5 = case string:find(LowerText, "efficient") =/= nomatch orelse
                string:find(LowerText, "optimize") =/= nomatch orelse
                string:find(LowerText, "improve") =/= nomatch of
        true -> [#{
            <<"original">> => <<"How to be more efficient">>,
            <<"inverted">> => <<"How to eliminate waste">>,
            <<"question">> => <<"What creates waste? Remove those sources.">>
        } | Inv4];
        false -> Inv4
    end,
    
    %% Add general inversion if no specific ones found
    FinalInversions = case Inv5 of
        [] -> [#{
            <<"original">> => <<"How to achieve the goal">>,
            <<"inverted">> => <<"How to guarantee failure">>,
            <<"question">> => <<"What would make this impossible? Avoid those things.">>
        }];
        _ -> Inv5
    end,
    
    #{
        <<"success">> => true,
        <<"inversions">> => FinalInversions,
        <<"inversion_count">> => length(FinalInversions),
        <<"munger_quote">> => <<"\"Invert, always invert.\" - Charlie Munger">>
    };

generate_inverted_perspective(_) ->
    #{<<"success">> => false, <<"error">> => <<"Invalid input">>}.

%%====================================================================
%% Internal functions
%%====================================================================

extract_decision_patterns(Text) ->
    Patterns = [],
    
    %% Check for either/or thinking
    P1 = case string:find(Text, "either") =/= nomatch orelse
              string:find(Text, "or") =/= nomatch of
        true -> [#{
            <<"type">> => <<"decision">>,
            <<"pattern">> => <<"Binary Thinking">>,
            <<"description">> => <<"Either/or framing detected - consider if there are more options">>,
            <<"confidence">> => 70
        } | Patterns];
        false -> Patterns
    end,
    
    %% Check for should/must language
    P2 = case string:find(Text, "should") =/= nomatch orelse
              string:find(Text, "must") =/= nomatch orelse
              string:find(Text, "have to") =/= nomatch of
        true -> [#{
            <<"type">> => <<"decision">>,
            <<"pattern">> => <<"Obligation Framing">>,
            <<"description">> => <<"Obligation language detected - question if this is truly required">>,
            <<"confidence">> => 65
        } | P1];
        false -> P1
    end,
    
    %% Check for risk language
    P3 = case string:find(Text, "risk") =/= nomatch orelse
              string:find(Text, "danger") =/= nomatch orelse
              string:find(Text, "threat") =/= nomatch of
        true -> [#{
            <<"type">> => <<"decision">>,
            <<"pattern">> => <<"Risk Awareness">>,
            <<"description">> => <<"Risk considerations present - ensure balanced risk assessment">>,
            <<"confidence">> => 75
        } | P2];
        false -> P2
    end,
    
    P3.

extract_thinking_patterns(Text) ->
    Patterns = [],
    
    %% Check for analytical thinking
    P1 = case string:find(Text, "analyze") =/= nomatch orelse
              string:find(Text, "examine") =/= nomatch orelse
              string:find(Text, "evaluate") =/= nomatch of
        true -> [#{
            <<"type">> => <<"thinking">>,
            <<"pattern">> => <<"Analytical Approach">>,
            <<"description">> => <<"Analytical thinking detected - good for complex problems">>,
            <<"confidence">> => 80
        } | Patterns];
        false -> Patterns
    end,
    
    %% Check for intuitive thinking
    P2 = case string:find(Text, "feel") =/= nomatch orelse
              string:find(Text, "sense") =/= nomatch orelse
              string:find(Text, "gut") =/= nomatch of
        true -> [#{
            <<"type">> => <<"thinking">>,
            <<"pattern">> => <<"Intuitive Approach">>,
            <<"description">> => <<"Intuitive thinking detected - validate with data when possible">>,
            <<"confidence">> => 60
        } | P1];
        false -> P1
    end,
    
    %% Check for systematic thinking
    P3 = case string:find(Text, "system") =/= nomatch orelse
              string:find(Text, "process") =/= nomatch orelse
              string:find(Text, "framework") =/= nomatch of
        true -> [#{
            <<"type">> => <<"thinking">>,
            <<"pattern">> => <<"Systems Thinking">>,
            <<"description">> => <<"Systems perspective detected - consider interconnections">>,
            <<"confidence">> => 85
        } | P2];
        false -> P2
    end,
    
    P3.

extract_emotional_patterns(Text) ->
    Patterns = [],
    
    %% Check for fear-based language
    P1 = case string:find(Text, "afraid") =/= nomatch orelse
              string:find(Text, "worry") =/= nomatch orelse
              string:find(Text, "concern") =/= nomatch of
        true -> [#{
            <<"type">> => <<"emotional">>,
            <<"pattern">> => <<"Fear-Based Thinking">>,
            <<"description">> => <<"Fear or worry detected - distinguish rational from irrational concerns">>,
            <<"confidence">> => 70
        } | Patterns];
        false -> Patterns
    end,
    
    %% Check for optimism
    P2 = case string:find(Text, "hope") =/= nomatch orelse
              string:find(Text, "optimistic") =/= nomatch orelse
              string:find(Text, "excited") =/= nomatch of
        true -> [#{
            <<"type">> => <<"emotional">>,
            <<"pattern">> => <<"Optimistic Outlook">>,
            <<"description">> => <<"Optimism detected - ensure it's grounded in reality">>,
            <<"confidence">> => 65
        } | P1];
        false -> P1
    end,
    
    %% Check for frustration
    P3 = case string:find(Text, "frustrat") =/= nomatch orelse
              string:find(Text, "annoy") =/= nomatch orelse
              string:find(Text, "problem") =/= nomatch of
        true -> [#{
            <<"type">> => <<"emotional">>,
            <<"pattern">> => <<"Frustration Signal">>,
            <<"description">> => <<"Frustration detected - identify root cause before solving">>,
            <<"confidence">> => 60
        } | P2];
        false -> P2
    end,
    
    P3.

extract_temporal_patterns(Text) ->
    Patterns = [],
    
    %% Check for urgency
    P1 = case string:find(Text, "urgent") =/= nomatch orelse
              string:find(Text, "immediately") =/= nomatch orelse
              string:find(Text, "asap") =/= nomatch of
        true -> [#{
            <<"type">> => <<"temporal">>,
            <<"pattern">> => <<"Urgency Bias">>,
            <<"description">> => <<"Urgency detected - verify if truly time-sensitive">>,
            <<"confidence">> => 75
        } | Patterns];
        false -> Patterns
    end,
    
    %% Check for long-term thinking
    P2 = case string:find(Text, "long-term") =/= nomatch orelse
              string:find(Text, "future") =/= nomatch orelse
              string:find(Text, "years") =/= nomatch of
        true -> [#{
            <<"type">> => <<"temporal">>,
            <<"pattern">> => <<"Long-Term Orientation">>,
            <<"description">> => <<"Long-term thinking detected - good for strategic decisions">>,
            <<"confidence">> => 80
        } | P1];
        false -> P1
    end,
    
    %% Check for short-term focus
    P3 = case string:find(Text, "now") =/= nomatch orelse
              string:find(Text, "today") =/= nomatch orelse
              string:find(Text, "quick") =/= nomatch of
        true -> [#{
            <<"type">> => <<"temporal">>,
            <<"pattern">> => <<"Short-Term Focus">>,
            <<"description">> => <<"Short-term focus detected - consider long-term implications">>,
            <<"confidence">> => 65
        } | P2];
        false -> P2
    end,
    
    P3.

%% Helper functions for insight extraction
has_contrast_pattern(Text) ->
    string:find(Text, "but") =/= nomatch orelse
    string:find(Text, "however") =/= nomatch orelse
    string:find(Text, "although") =/= nomatch orelse
    string:find(Text, "despite") =/= nomatch.

has_causal_pattern(Text) ->
    string:find(Text, "because") =/= nomatch orelse
    string:find(Text, "therefore") =/= nomatch orelse
    string:find(Text, "thus") =/= nomatch orelse
    string:find(Text, "consequently") =/= nomatch.

has_uncertainty_pattern(Text) ->
    string:find(Text, "maybe") =/= nomatch orelse
    string:find(Text, "might") =/= nomatch orelse
    string:find(Text, "could") =/= nomatch orelse
    string:find(Text, "perhaps") =/= nomatch.

has_absolute_pattern(Text) ->
    string:find(Text, "always") =/= nomatch orelse
    string:find(Text, "never") =/= nomatch orelse
    string:find(Text, "everyone") =/= nomatch orelse
    string:find(Text, "no one") =/= nomatch.

has_comparison_pattern(Text) ->
    string:find(Text, "better") =/= nomatch orelse
    string:find(Text, "worse") =/= nomatch orelse
    string:find(Text, "more than") =/= nomatch orelse
    string:find(Text, "less than") =/= nomatch.
