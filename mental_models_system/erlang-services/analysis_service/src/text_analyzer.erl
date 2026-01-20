%%%-------------------------------------------------------------------
%%% @doc Text Analyzer - Keyword-based Mental Model Detection
%%% 
%%% Orchestrates detection across multiple detector modules:
%%% - detectors_science: Math, Physics, Engineering, Biology
%%% - detectors_psychology: Biases and cognitive patterns
%%% - detectors_business: Economics, Porter's, Moat, etc.
%%% - detectors_thinking: Inversion, First Principles, etc.
%%% @end
%%%-------------------------------------------------------------------
-module(text_analyzer).

-export([analyze_text/1, detect_all_models/1, detect_model/2, 
         top_detected_models/2, detect_with_threshold/2]).

%%====================================================================
%% API
%%====================================================================

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

detect_all_models(Text) ->
    Detectors = get_all_detectors(),
    [Detector(Text) || Detector <- Detectors].

detect_model(Text, ModelSlug) ->
    Detectors = get_detector_map(),
    case maps:get(ModelSlug, Detectors, undefined) of
        undefined -> {error, unknown_model};
        Detector -> Detector(Text)
    end.

top_detected_models(Text, N) ->
    Models = detect_all_models(Text),
    Sorted = lists:sort(fun(A, B) ->
        maps:get(<<"score">>, A, 0) >= maps:get(<<"score">>, B, 0)
    end, Models),
    lists:sublist(Sorted, N).

detect_with_threshold(Text, Threshold) ->
    Models = detect_all_models(Text),
    [M || M <- Models, maps:get(<<"score">>, M, 0) >= Threshold].

%%====================================================================
%% Internal - Detector Registry
%%====================================================================

get_all_detectors() ->
    %% Science detectors
    [fun detectors_science:detect_mathematics/1,
     fun detectors_science:detect_physics/1,
     fun detectors_science:detect_engineering/1,
     fun detectors_science:detect_biology/1,
     %% Psychology detectors
     fun detectors_psychology:detect_psychology/1,
     fun detectors_psychology:detect_confirmation_bias/1,
     fun detectors_psychology:detect_anchoring_bias/1,
     fun detectors_psychology:detect_availability_heuristic/1,
     fun detectors_psychology:detect_overconfidence_bias/1,
     fun detectors_psychology:detect_loss_aversion/1,
     fun detectors_psychology:detect_sunk_cost_fallacy/1,
     %% Business detectors
     fun detectors_business:detect_economics/1,
     fun detectors_business:detect_porters_five_forces/1,
     fun detectors_business:detect_economic_moat/1,
     fun detectors_business:detect_compound_interest/1,
     fun detectors_business:detect_network_effects/1,
     fun detectors_business:detect_margin_of_safety/1,
     %% Thinking detectors
     fun detectors_thinking:detect_inversion/1,
     fun detectors_thinking:detect_first_principles/1,
     fun detectors_thinking:detect_second_order_thinking/1].

get_detector_map() ->
    #{
        <<"mathematics">> => fun detectors_science:detect_mathematics/1,
        <<"physics">> => fun detectors_science:detect_physics/1,
        <<"engineering">> => fun detectors_science:detect_engineering/1,
        <<"biology">> => fun detectors_science:detect_biology/1,
        <<"psychology">> => fun detectors_psychology:detect_psychology/1,
        <<"confirmation_bias">> => fun detectors_psychology:detect_confirmation_bias/1,
        <<"inversion">> => fun detectors_thinking:detect_inversion/1,
        <<"first_principles">> => fun detectors_thinking:detect_first_principles/1
    }.
