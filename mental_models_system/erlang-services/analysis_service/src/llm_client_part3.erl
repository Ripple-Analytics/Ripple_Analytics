%%%-------------------------------------------------------------------
%%% @doc llm_client Helper Module - Part 3
%%% @end
%%%-------------------------------------------------------------------
-module(llm_client_part3).

-export([keyword_analyze/2, keyword_detect_biases/1]).

keyword_analyze(Text, TopN) ->
    Models = model_registry:get_all_models(),
    TextLower = string:lowercase(binary_to_list(Text)),
    
    %% Score each model based on keyword matches
    Scored = lists:map(fun(Model) ->
        Keywords = maps:get(<<"keywords">>, Model, []),
        Score = lists:foldl(fun(Keyword, Acc) ->
            KeywordStr = string:lowercase(binary_to_list(Keyword)),
            case string:find(TextLower, KeywordStr) of
                nomatch -> Acc;
                _ -> Acc + 10
            end
        end, 0, Keywords),
        {Score, Model}
    end, Models),
    
    %% Sort by score and take top N
    Sorted = lists:reverse(lists:keysort(1, Scored)),
    TopModels = lists:sublist(Sorted, TopN),
    
    Results = lists:map(fun({Score, Model}) ->
        #{
            <<"name">> => maps:get(<<"name">>, Model, <<>>),
            <<"category">> => maps:get(<<"category">>, Model, <<>>),
            <<"relevance">> => Score,
            <<"description">> => maps:get(<<"description">>, Model, <<>>)
        }
    end, TopModels),
    
    #{
        <<"success">> => true,
        <<"models">> => Results,
        <<"llm_powered">> => false,
        <<"method">> => <<"keyword_matching">>
    }.

keyword_detect_biases(Text) ->
    BiasKeywords = [
        %% Judgment Biases
        {<<"confirmation_bias">>, [<<"confirms">>, <<"proves">>, <<"knew it">>, <<"always said">>, <<"evidence supports">>, <<"validates">>]},
        {<<"anchoring">>, [<<"first">>, <<"original">>, <<"started at">>, <<"initial">>, <<"reference point">>, <<"baseline">>]},
        {<<"availability_heuristic">>, [<<"recently">>, <<"just saw">>, <<"heard about">>, <<"news">>, <<"remember when">>, <<"last time">>]},
        {<<"hindsight_bias">>, [<<"obvious">>, <<"should have known">>, <<"predictable">>, <<"saw it coming">>, <<"inevitable">>]},
        {<<"recency_bias">>, [<<"lately">>, <<"recent">>, <<"just happened">>, <<"this week">>, <<"trending now">>]},
        %% Decision Biases
        {<<"loss_aversion">>, [<<"can't lose">>, <<"risk">>, <<"afraid">>, <<"protect">>, <<"downside">>, <<"worst case">>]},
        {<<"sunk_cost_fallacy">>, [<<"already invested">>, <<"come this far">>, <<"too late to stop">>, <<"wasted if">>, <<"put so much">>]},
        {<<"status_quo_bias">>, [<<"always done">>, <<"why change">>, <<"working fine">>, <<"tradition">>, <<"usual way">>]},
        %% Social Biases
        {<<"social_proof">>, [<<"everyone">>, <<"popular">>, <<"trending">>, <<"others">>, <<"majority">>, <<"most people">>]},
        {<<"authority_bias">>, [<<"expert says">>, <<"according to">>, <<"studies show">>, <<"research proves">>, <<"scientists">>]},
        %% Self-Assessment Biases
        {<<"dunning_kruger">>, [<<"easy">>, <<"simple">>, <<"anyone can">>, <<"obvious solution">>, <<"no big deal">>]},
        {<<"overconfidence">>, [<<"definitely">>, <<"guaranteed">>, <<"100%">>, <<"certain">>, <<"no doubt">>, <<"absolutely">>]}
    ],
    
    TextLower = string:lowercase(binary_to_list(Text)),
    
    DetectedBiases = lists:filtermap(fun({BiasName, Keywords}) ->
        Matches = lists:filter(fun(Keyword) ->
            KeywordStr = string:lowercase(binary_to_list(Keyword)),
            string:find(TextLower, KeywordStr) =/= nomatch
        end, Keywords),
        case Matches of
            [] -> false;
            _ -> {true, #{
                <<"bias">> => BiasName,
                <<"evidence">> => Matches,
                <<"severity">> => case length(Matches) of
                    1 -> <<"low">>;
                    2 -> <<"medium">>;
                    _ -> <<"high">>
                end
            }}
        end
    end, BiasKeywords),
    
    #{
        <<"success">> => true,
        <<"biases">> => DetectedBiases,
        <<"llm_powered">> => false,
        <<"method">> => <<"keyword_matching">>
    }.

