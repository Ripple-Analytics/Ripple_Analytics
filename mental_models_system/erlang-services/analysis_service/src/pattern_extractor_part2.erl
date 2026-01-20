%%%-------------------------------------------------------------------
%%% @doc pattern_extractor Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(pattern_extractor_part2).

-export([generate_inverted_perspective/1, generate_inverted_perspective/1, extract_decision_patterns/1]).

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

