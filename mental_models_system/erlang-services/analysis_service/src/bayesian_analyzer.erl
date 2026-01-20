%%%-------------------------------------------------------------------
%%% @doc Bayesian Analyzer
%%% 
%%% Implements Bayesian inference for mental model confidence scoring.
%%% Uses Bayes' theorem to update beliefs about which mental models
%%% apply to a given text based on evidence (keywords, patterns).
%%%
%%% P(Model|Evidence) = P(Evidence|Model) * P(Model) / P(Evidence)
%%%
%%% This allows us to:
%%% 1. Start with prior beliefs about model applicability
%%% 2. Update beliefs as we find evidence in the text
%%% 3. Calculate confidence intervals for our predictions
%%% @end
%%%-------------------------------------------------------------------
-module(bayesian_analyzer).
%% Helper modules: bayesian_analyzer_part2

-export([
    analyze/2,
    calculate_posterior/3,
    update_belief/3,
    batch_update_beliefs/2,
    confidence_interval/2,
    has_converged/2
]).

%% Default prior probability for any mental model
-define(DEFAULT_PRIOR, 0.1).

%% Minimum probability to avoid numerical issues
-define(MIN_PROB, 0.001).

%% Maximum probability
-define(MAX_PROB, 0.999).

%%====================================================================
%% API
%%====================================================================

%% @doc Analyze text using Bayesian inference
%% Returns models with posterior probabilities
analyze(Text, Models) when is_binary(Text), is_list(Models) ->
    %% Extract evidence from text
    Evidence = extract_evidence(Text),
    
    %% Calculate posteriors for each model
    Results = lists:map(fun(Model) ->
        ModelName = maps:get(<<"name">>, Model, <<>>),
        Keywords = maps:get(<<"keywords">>, Model, []),
        
        %% Calculate prior (base rate for this model category)
        Prior = calculate_prior(Model),
        
        %% Calculate likelihood based on keyword matches
        Likelihood = calculate_likelihood(Keywords, Evidence),
        
        %% Calculate posterior using Bayes' theorem
        Posterior = calculate_posterior(Prior, Likelihood, Evidence),
        
        %% Calculate confidence interval
        {Lower, Upper} = confidence_interval(Posterior, length(Evidence)),
        
        Model#{
            <<"bayesian_score">> => round(Posterior * 1000) / 10,
            <<"prior">> => round(Prior * 1000) / 10,
            <<"likelihood">> => round(Likelihood * 1000) / 10,
            <<"confidence_lower">> => round(Lower * 1000) / 10,
            <<"confidence_upper">> => round(Upper * 1000) / 10,
            <<"evidence_count">> => count_matches(Keywords, Evidence)
        }
    end, Models),
    
    %% Sort by posterior probability
    Sorted = lists:sort(fun(A, B) ->
        maps:get(<<"bayesian_score">>, A, 0) >= maps:get(<<"bayesian_score">>, B, 0)
    end, Results),
    
    #{
        <<"success">> => true,
        <<"models">> => Sorted,
        <<"evidence_extracted">> => length(Evidence),
        <<"method">> => <<"bayesian_inference">>
    }.

%% @doc Calculate posterior probability using Bayes' theorem
%% P(H|E) = P(E|H) * P(H) / P(E)
calculate_posterior(Prior, Likelihood, Evidence) when is_list(Evidence) ->
    %% P(E) - marginal likelihood (evidence probability)
    %% Simplified: assume evidence is equally likely under any hypothesis
    MarginalLikelihood = max(?MIN_PROB, 0.5),
    
    %% Apply Bayes' theorem
    Numerator = Likelihood * Prior,
    Posterior = Numerator / MarginalLikelihood,
    
    %% Clamp to valid probability range
    clamp(Posterior, ?MIN_PROB, ?MAX_PROB);

calculate_posterior(Prior, Likelihood, _) ->
    clamp(Likelihood * Prior / 0.5, ?MIN_PROB, ?MAX_PROB).

%% @doc Update belief about a model given new evidence
%% Implements sequential Bayesian updating
update_belief(CurrentBelief, NewEvidence, Model) ->
    Keywords = maps:get(<<"keywords">>, Model, []),
    Likelihood = calculate_likelihood(Keywords, [NewEvidence]),
    
    %% Sequential update: new posterior = old posterior * likelihood / normalizer
    Numerator = CurrentBelief * Likelihood,
    Denominator = CurrentBelief * Likelihood + (1 - CurrentBelief) * (1 - Likelihood),
    
    case Denominator of
        0 -> CurrentBelief;
        _ -> clamp(Numerator / Denominator, ?MIN_PROB, ?MAX_PROB)
    end.

%% @doc Batch update beliefs for multiple pieces of evidence
batch_update_beliefs(InitialBeliefs, EvidenceList) when is_list(EvidenceList) ->
    lists:foldl(fun(Evidence, Beliefs) ->
        maps:map(fun(ModelName, {Belief, Model}) ->
            NewBelief = update_belief(Belief, Evidence, Model),
            {NewBelief, Model}
        end, Beliefs)
    end, InitialBeliefs, EvidenceList).

%% @doc Calculate 95% confidence interval for posterior
%% Uses normal approximation for binomial proportion
confidence_interval(Posterior, SampleSize) when SampleSize > 0 ->
    %% Standard error using normal approximation
    SE = math:sqrt(Posterior * (1 - Posterior) / max(1, SampleSize)),
    
    %% 95% CI uses z = 1.96
    Z = 1.96,
    Margin = Z * SE,
    
    Lower = clamp(Posterior - Margin, ?MIN_PROB, ?MAX_PROB),
    Upper = clamp(Posterior + Margin, ?MIN_PROB, ?MAX_PROB),
    
    {Lower, Upper};

confidence_interval(Posterior, _) ->
    %% No sample, wide confidence interval
    {?MIN_PROB, ?MAX_PROB}.

%% @doc Check if beliefs have converged (change < threshold)