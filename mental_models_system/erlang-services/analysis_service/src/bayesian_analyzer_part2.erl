%%%-------------------------------------------------------------------
%%% @doc bayesian_analyzer Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(bayesian_analyzer_part2).

-export([has_converged/2, has_converged/2, extract_evidence/1, extract_evidence/1, extract_bigrams/1, extract_bigrams/1, extract_bigrams/2, calculate_prior/1, calculate_likelihood/2, calculate_likelihood/2, count_matches/2, count_matches/2, clamp/3]).

has_converged(OldBeliefs, NewBeliefs) when is_map(OldBeliefs), is_map(NewBeliefs) ->
    Threshold = 0.01,
    
    Changes = maps:fold(fun(Key, NewVal, Acc) ->
        OldVal = maps:get(Key, OldBeliefs, 0),
        [abs(NewVal - OldVal) | Acc]
    end, [], NewBeliefs),
    
    case Changes of
        [] -> true;
        _ -> lists:max(Changes) < Threshold
    end;

has_converged(_, _) ->
    false.

%%====================================================================
%% Internal functions
%%====================================================================

%% Extract evidence (keywords/patterns) from text
extract_evidence(Text) when is_binary(Text) ->
    %% Convert to lowercase for matching
    LowerText = string:lowercase(binary_to_list(Text)),
    
    %% Split into words
    Words = string:tokens(LowerText, " \t\n\r.,;:!?\"'()-[]{}"),
    
    %% Also extract bigrams for phrase matching
    Bigrams = extract_bigrams(Words),
    
    %% Combine words and bigrams as evidence
    lists:usort(Words ++ Bigrams);

extract_evidence(_) ->
    [].

extract_bigrams([]) -> [];
extract_bigrams([_]) -> [];
extract_bigrams([W1, W2 | Rest]) ->
    [W1 ++ " " ++ W2 | extract_bigrams([W2 | Rest])].

%% Calculate prior probability based on model category
calculate_prior(Model) ->
    Category = maps:get(<<"category">>, Model, <<>>),
    
    %% Different categories have different base rates
    %% These are rough estimates based on how often these models apply
    CategoryPriors = #{
        <<"Thinking Tools">> => 0.15,
        <<"Psychology">> => 0.12,
        <<"Economics">> => 0.10,
        <<"Systems">> => 0.08,
        <<"Physics">> => 0.05,
        <<"Biology">> => 0.05,
        <<"Mathematics">> => 0.06,
        <<"Organizational">> => 0.08
    },
    
    maps:get(Category, CategoryPriors, ?DEFAULT_PRIOR).

%% Calculate likelihood P(Evidence|Model)
calculate_likelihood(Keywords, Evidence) when is_list(Keywords), is_list(Evidence) ->
    case Keywords of
        [] -> ?DEFAULT_PRIOR;
        _ ->
            %% Count how many keywords match the evidence
            Matches = count_matches(Keywords, Evidence),
            TotalKeywords = length(Keywords),
            
            %% Likelihood increases with more matches
            %% Use a smoothed ratio to avoid 0 or 1
            MatchRatio = Matches / max(1, TotalKeywords),
            
            %% Apply sigmoid-like transformation for smoother probabilities
            %% This maps [0,1] -> [0.1, 0.9] roughly
            0.1 + 0.8 * MatchRatio
    end;

calculate_likelihood(_, _) ->
    ?DEFAULT_PRIOR.

%% Count keyword matches in evidence
count_matches(Keywords, Evidence) when is_list(Keywords), is_list(Evidence) ->
    EvidenceSet = sets:from_list(Evidence),
    
    lists:foldl(fun(Keyword, Count) ->
        KeywordStr = case Keyword of
            B when is_binary(B) -> string:lowercase(binary_to_list(B));
            L when is_list(L) -> string:lowercase(L);
            _ -> ""
        end,
        
        case sets:is_element(KeywordStr, EvidenceSet) of
            true -> Count + 1;
            false -> 
                %% Also check for partial matches
                case lists:any(fun(E) -> 
                    string:find(E, KeywordStr) =/= nomatch orelse
                    string:find(KeywordStr, E) =/= nomatch
                end, Evidence) of
                    true -> Count + 0.5;
                    false -> Count
                end
        end
    end, 0, Keywords);

count_matches(_, _) ->
    0.

%% Clamp value to range
clamp(Value, Min, Max) ->
    max(Min, min(Max, Value)).

