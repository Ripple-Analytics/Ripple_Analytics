%%%-------------------------------------------------------------------
%%% @doc Text Analyzer - Keyword-based Mental Model Detection
%%% 
%%% Ports the detection algorithms from Clojure to Erlang.
%%% Uses keyword matching, pattern detection, and linguistic analysis
%%% to identify mental models in text.
%%%
%%% Based on algorithms from feature/mental-model-algorithms-psychology
%%% @end
%%%-------------------------------------------------------------------
-module(text_analyzer).
%% Helper modules: text_analyzer_part2, text_analyzer_part3, text_analyzer_part4, text_analyzer_part5

-export([analyze_text/1, detect_all_models/1, detect_model/2, 
         top_detected_models/2, detect_with_threshold/2]).

%%====================================================================
%% API
%%====================================================================

%% @doc Analyze text and return all detected mental models with scores
analyze_text(Text) when is_binary(Text) ->
    Models = detect_all_models(Text),
    SortedModels = lists:sort(fun(A, B) ->
        maps:get(<<"score">>, A, 0) >= maps:get(<<"score">>, B, 0)
    end, Models),
    #{
        <<"success">> => true,
        <<"text_length">> => byte_size(Text),
        <<"models">> => SortedModels,
        <<"top_models">> => lists:sublist(SortedModels, 5)
    }.

%% @doc Detect all mental models in text
detect_all_models(Text) ->
    Detectors = [
        {<<"mathematics">>, fun detect_mathematics/1},
        {<<"physics">>, fun detect_physics/1},
        {<<"engineering">>, fun detect_engineering/1},
        {<<"biology">>, fun detect_biology/1},
        {<<"psychology">>, fun detect_psychology/1},
        {<<"economics">>, fun detect_economics/1},
        {<<"confirmation_bias">>, fun detect_confirmation_bias/1},
        {<<"anchoring_bias">>, fun detect_anchoring_bias/1},
        {<<"availability_heuristic">>, fun detect_availability_heuristic/1},
        {<<"overconfidence_bias">>, fun detect_overconfidence_bias/1},
        {<<"loss_aversion">>, fun detect_loss_aversion/1},
        {<<"sunk_cost_fallacy">>, fun detect_sunk_cost_fallacy/1},
        {<<"porters_five_forces">>, fun detect_porters_five_forces/1},
        {<<"economic_moat">>, fun detect_economic_moat/1},
        {<<"inversion">>, fun detect_inversion/1},
        {<<"first_principles">>, fun detect_first_principles/1},
        {<<"second_order_thinking">>, fun detect_second_order_thinking/1},
        {<<"margin_of_safety">>, fun detect_margin_of_safety/1},
        {<<"compound_interest">>, fun detect_compound_interest/1},
        {<<"network_effects">>, fun detect_network_effects/1}
    ],
    [begin
        Result = Detector(Text),
        maps:put(<<"model_slug">>, Slug, Result)
    end || {Slug, Detector} <- Detectors].

%% @doc Detect a specific model in text
detect_model(Text, ModelSlug) ->
    Detectors = #{
        <<"mathematics">> => fun detect_mathematics/1,
        <<"physics">> => fun detect_physics/1,
        <<"engineering">> => fun detect_engineering/1,
        <<"confirmation_bias">> => fun detect_confirmation_bias/1,
        <<"inversion">> => fun detect_inversion/1
    },
    case maps:get(ModelSlug, Detectors, undefined) of
        undefined -> {error, unknown_model};
        Detector -> Detector(Text)
    end.

%% @doc Get top N detected models
top_detected_models(Text, N) ->
    Models = detect_all_models(Text),
    Sorted = lists:sort(fun(A, B) ->
        maps:get(<<"score">>, A, 0) >= maps:get(<<"score">>, B, 0)
    end, Models),
    lists:sublist(Sorted, N).

%% @doc Get models above confidence threshold
detect_with_threshold(Text, Threshold) ->
    Models = detect_all_models(Text),
    [M || M <- Models, maps:get(<<"score">>, M, 0) >= Threshold].

%%====================================================================
%% Detection Algorithms - Hard Sciences
%%====================================================================

detect_mathematics(Text) ->
    Keywords = [<<"equation">>, <<"formula">>, <<"calculation">>, <<"proof">>, 
                <<"theorem">>, <<"logic">>, <<"number">>, <<"variable">>, 
                <<"function">>, <<"algorithm">>, <<"sequence">>, <<"pattern">>,
                <<"probability">>, <<"statistics">>, <<"ratio">>, <<"percentage">>],
    MathSymbols = [<<"=">>, <<"+">>, <<"-">>, <<"*">>, <<"/">>, <<"sqrt">>, <<"sum">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    SymbolCount = count_keywords(Text, MathSymbols),
    NumberCount = count_numbers(Text),
    
    BaseScore = (KeywordCount * 0.05) + (SymbolCount * 0.1) + (min(NumberCount, 10) * 0.02),
    Score = min(1.0, BaseScore / 0.5),
    
    #{
        <<"name">> => <<"Mathematics">>,
        <<"category">> => <<"Hard Sciences">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{
            <<"keywords">> => KeywordCount,
            <<"symbols">> => SymbolCount,
            <<"numbers">> => NumberCount
        },
        <<"confidence">> => confidence_level(Score)
    }.

detect_physics(Text) ->
    Keywords = [<<"force">>, <<"energy">>, <<"velocity">>, <<"acceleration">>, 
                <<"momentum">>, <<"gravity">>, <<"motion">>, <<"wave">>, 
                <<"particle">>, <<"quantum">>, <<"relativity">>, <<"thermodynamics">>,
                <<"entropy">>, <<"friction">>, <<"pressure">>, <<"temperature">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    BaseScore = KeywordCount * 0.06,
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Physics">>,
        <<"category">> => <<"Hard Sciences">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount},
        <<"confidence">> => confidence_level(Score)
    }.
