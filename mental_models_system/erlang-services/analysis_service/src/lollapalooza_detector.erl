%%%-------------------------------------------------------------------
%%% @doc Lollapalooza Detector
%%% 
%%% Detects Lollapalooza effects - when 3+ mental models converge
%%% with high relevance scores (>70%), indicating a powerful
%%% confluence of forces.
%%%
%%% Named after Charlie Munger's concept of multiple mental models
%%% combining to create outsized effects.
%%% @end
%%%-------------------------------------------------------------------
-module(lollapalooza_detector).

-export([detect/1, analyze_comprehensive/2, calculate_convergence/1]).

-define(LOLLAPALOOZA_THRESHOLD, 3).
-define(HIGH_SCORE_THRESHOLD, 70).

%%====================================================================
%% API
%%====================================================================

%% @doc Detect Lollapalooza effect from analysis results
%% Returns {true, Details} if 3+ models score > 70%
detect(AnalysisResults) when is_list(AnalysisResults) ->
    %% Filter to high-scoring models
    HighScoring = lists:filter(fun(Model) ->
        Score = maps:get(<<"relevance">>, Model, 0),
        Score >= ?HIGH_SCORE_THRESHOLD
    end, AnalysisResults),
    
    Count = length(HighScoring),
    IsLollapalooza = Count >= ?LOLLAPALOOZA_THRESHOLD,
    
    %% Calculate convergence score (average of high-scoring models)
    ConvergenceScore = case HighScoring of
        [] -> 0;
        _ -> 
            Total = lists:foldl(fun(M, Acc) -> 
                Acc + maps:get(<<"relevance">>, M, 0) 
            end, 0, HighScoring),
            Total / Count
    end,
    
    %% Group by category to find cross-domain convergence
    Categories = lists:usort([maps:get(<<"category">>, M, <<"Unknown">>) || M <- HighScoring]),
    CrossDomain = length(Categories) >= 2,
    
    #{
        <<"detected">> => IsLollapalooza,
        <<"convergence_count">> => Count,
        <<"convergence_score">> => round(ConvergenceScore * 10) / 10,
        <<"cross_domain">> => CrossDomain,
        <<"categories_involved">> => Categories,
        <<"converging_models">> => [maps:get(<<"name">>, M, <<"Unknown">>) || M <- HighScoring],
        <<"strength">> => calculate_strength(Count, ConvergenceScore, CrossDomain)
    };

detect(AnalysisResults) when is_map(AnalysisResults) ->
    Models = maps:get(<<"models">>, AnalysisResults, 
             maps:get(<<"analysis">>, AnalysisResults, [])),
    detect(Models).

%% @doc Calculate convergence metrics
calculate_convergence(Models) when is_list(Models) ->
    %% Score distribution
    Scores = [maps:get(<<"relevance">>, M, 0) || M <- Models],
    
    %% Calculate statistics
    Mean = case Scores of
        [] -> 0;
        _ -> lists:sum(Scores) / length(Scores)
    end,
    
    Max = case Scores of
        [] -> 0;
        _ -> lists:max(Scores)
    end,
    
    Min = case Scores of
        [] -> 0;
        _ -> lists:min(Scores)
    end,
    
    %% Variance calculation
    Variance = case Scores of
        [] -> 0;
        _ ->
            SqDiffs = [(S - Mean) * (S - Mean) || S <- Scores],
            lists:sum(SqDiffs) / length(Scores)
    end,
    
    #{
        <<"mean_score">> => round(Mean * 10) / 10,
        <<"max_score">> => Max,
        <<"min_score">> => Min,
        <<"variance">> => round(Variance * 10) / 10,
        <<"total_models">> => length(Models),
        <<"high_scoring">> => length([S || S <- Scores, S >= ?HIGH_SCORE_THRESHOLD])
    }.

%% @doc Run comprehensive analysis with Lollapalooza detection
analyze_comprehensive(Text, TopN) ->
    %% Get base analysis from LLM client
    BaseResult = llm_client:analyze(Text, TopN),
    
    %% Extract models from result
    Models = case BaseResult of
        {ok, Data} -> 
            maps:get(<<"models">>, Data, 
            maps:get(<<"analysis">>, Data, []));
        Data when is_map(Data) ->
            maps:get(<<"models">>, Data, 
            maps:get(<<"analysis">>, Data, []))
    end,
    
    %% Detect Lollapalooza
    Lollapalooza = detect(Models),
    
    %% Calculate convergence metrics
    Convergence = calculate_convergence(Models),
    
    %% Detect potential failure modes
    FailureModes = detect_failure_modes(Models),
    
    %% Build comprehensive result
    #{
        <<"success">> => true,
        <<"text_length">> => byte_size(Text),
        <<"models_analyzed">> => TopN,
        <<"analysis">> => Models,
        <<"lollapalooza">> => Lollapalooza,
        <<"convergence">> => Convergence,
        <<"failure_modes">> => FailureModes,
        <<"recommendations">> => generate_recommendations(Models, Lollapalooza)
    }.

%%====================================================================
%% Internal functions
%%====================================================================

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
