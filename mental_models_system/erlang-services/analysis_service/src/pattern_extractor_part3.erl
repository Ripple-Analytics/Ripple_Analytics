%%%-------------------------------------------------------------------
%%% @doc pattern_extractor Helper Module - Part 3
%%% @end
%%%-------------------------------------------------------------------
-module(pattern_extractor_part3).

-export([extract_thinking_patterns/1, extract_emotional_patterns/1, extract_temporal_patterns/1, has_contrast_pattern/1, has_causal_pattern/1]).

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

