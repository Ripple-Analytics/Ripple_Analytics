%%%-------------------------------------------------------------------
%%% @doc lollapalooza_detector Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(lollapalooza_detector_part2).

-export([calculate_strength/3, detect_failure_modes/1, generate_recommendations/2, add_model_recommendation/2, add_model_recommendation/2, add_model_recommendation/2, add_model_recommendation/2, add_model_recommendation/2]).

calculate_strength(Count, Score, CrossDomain) ->
    BaseStrength = case Count of
        N when N >= 5 -> <<"extreme">>;
        N when N >= 4 -> <<"very_strong">>;
        N when N >= 3 -> <<"strong">>;
        _ -> <<"moderate">>
    end,
    
    %% Boost if cross-domain and high score
    case {CrossDomain, Score >= 80} of
        {true, true} -> 
            case BaseStrength of
                <<"strong">> -> <<"very_strong">>;
                <<"very_strong">> -> <<"extreme">>;
                Other -> Other
            end;
        _ -> BaseStrength
    end.

detect_failure_modes(Models) ->
    %% Map models to potential failure modes
    FailureMappings = #{
        <<"Confirmation Bias">> => [
            #{<<"mode">> => <<"Echo Chamber">>, <<"risk">> => <<"high">>},
            #{<<"mode">> => <<"Missed Signals">>, <<"risk">> => <<"high">>}
        ],
        <<"Anchoring">> => [
            #{<<"mode">> => <<"Suboptimal Decisions">>, <<"risk">> => <<"medium">>}
        ],
        <<"Overconfidence">> => [
            #{<<"mode">> => <<"Excessive Risk Taking">>, <<"risk">> => <<"critical">>}
        ],
        <<"Sunk Cost Fallacy">> => [
            #{<<"mode">> => <<"Wasted Resources">>, <<"risk">> => <<"high">>},
            #{<<"mode">> => <<"Compounding Losses">>, <<"risk">> => <<"critical">>}
        ],
        <<"Loss Aversion">> => [
            #{<<"mode">> => <<"Missed Opportunities">>, <<"risk">> => <<"medium">>}
        ],
        <<"Status Quo Bias">> => [
            #{<<"mode">> => <<"Stagnation">>, <<"risk">> => <<"medium">>}
        ]
    },
    
    %% Find failure modes for detected models
    DetectedModes = lists:flatmap(fun(Model) ->
        Name = maps:get(<<"name">>, Model, <<>>),
        Score = maps:get(<<"relevance">>, Model, 0),
        Modes = maps:get(Name, FailureMappings, []),
        [maps:put(<<"source_model">>, Name, 
         maps:put(<<"model_score">>, Score, M)) || M <- Modes]
    end, Models),
    
    %% Sort by risk level
    RiskOrder = #{<<"critical">> => 1, <<"high">> => 2, <<"medium">> => 3, <<"low">> => 4},
    lists:sort(fun(A, B) ->
        RiskA = maps:get(maps:get(<<"risk">>, A, <<"low">>), RiskOrder, 5),
        RiskB = maps:get(maps:get(<<"risk">>, B, <<"low">>), RiskOrder, 5),
        RiskA =< RiskB
    end, DetectedModes).

generate_recommendations(Models, Lollapalooza) ->
    Recommendations = [],
    
    %% Add Lollapalooza-specific recommendations
    R1 = case maps:get(<<"detected">>, Lollapalooza, false) of
        true ->
            Strength = maps:get(<<"strength">>, Lollapalooza, <<"moderate">>),
            [#{
                <<"type">> => <<"lollapalooza_alert">>,
                <<"priority">> => <<"high">>,
                <<"message">> => iolist_to_binary([
                    <<"Multiple mental models (">>,
                    integer_to_binary(maps:get(<<"convergence_count">>, Lollapalooza, 0)),
                    <<") are converging with ">>,
                    Strength,
                    <<" intensity. This suggests a significant decision point.">>
                ]),
                <<"action">> => <<"Consider the combined implications of all converging models before deciding.">>
            } | Recommendations];
        false ->
            Recommendations
    end,
    
    %% Add cross-domain recommendations
    R2 = case maps:get(<<"cross_domain">>, Lollapalooza, false) of
        true ->
            Categories = maps:get(<<"categories_involved">>, Lollapalooza, []),
            [#{
                <<"type">> => <<"cross_domain_insight">>,
                <<"priority">> => <<"medium">>,
                <<"message">> => iolist_to_binary([
                    <<"Analysis spans multiple domains: ">>,
                    lists:join(<<", ">>, Categories),
                    <<". Cross-domain thinking often reveals hidden connections.">>
                ]),
                <<"action">> => <<"Look for unexpected interactions between different model categories.">>
            } | R1];
        false ->
            R1
    end,
    
    %% Add model-specific recommendations based on top models
    TopModels = lists:sublist(Models, 3),
    R3 = lists:foldl(fun(Model, Acc) ->
        Name = maps:get(<<"name">>, Model, <<>>),
        add_model_recommendation(Name, Acc)
    end, R2, TopModels),
    
    R3.

add_model_recommendation(<<"Inversion">>, Acc) ->
    [#{
        <<"type">> => <<"thinking_tool">>,
        <<"priority">> => <<"medium">>,
        <<"message">> => <<"Inversion detected - consider what could go wrong.">>,
        <<"action">> => <<"List the top 3 ways this decision could fail, then plan to avoid them.">>
    } | Acc];
add_model_recommendation(<<"First Principles Thinking">>, Acc) ->
    [#{
        <<"type">> => <<"thinking_tool">>,
        <<"priority">> => <<"medium">>,
        <<"message">> => <<"First principles thinking detected - challenge assumptions.">>,
        <<"action">> => <<"Identify and question the fundamental assumptions underlying this situation.">>
    } | Acc];
add_model_recommendation(<<"Confirmation Bias">>, Acc) ->
    [#{
        <<"type">> => <<"bias_warning">>,
        <<"priority">> => <<"high">>,
        <<"message">> => <<"Confirmation bias detected - seek disconfirming evidence.">>,
        <<"action">> => <<"Actively look for information that contradicts your current view.">>
    } | Acc];
add_model_recommendation(<<"Sunk Cost Fallacy">>, Acc) ->
    [#{
        <<"type">> => <<"bias_warning">>,
        <<"priority">> => <<"high">>,
        <<"message">> => <<"Sunk cost thinking detected - focus on future value only.">>,
        <<"action">> => <<"Ignore past investments and evaluate this decision based solely on future outcomes.">>
    } | Acc];
add_model_recommendation(_, Acc) ->
    Acc.

