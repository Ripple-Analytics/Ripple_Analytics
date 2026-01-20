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
%% Helper modules: lollapalooza_detector_part2

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
