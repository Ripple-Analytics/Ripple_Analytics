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

detect_engineering(Text) ->
    Keywords = [<<"design">>, <<"system">>, <<"optimization">>, <<"efficiency">>, 
                <<"constraint">>, <<"trade-off">>, <<"specification">>, <<"tolerance">>,
                <<"failure">>, <<"redundancy">>, <<"feedback">>, <<"control">>,
                <<"simulation">>, <<"prototype">>, <<"testing">>, <<"requirement">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    BaseScore = KeywordCount * 0.05,
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Engineering">>,
        <<"category">> => <<"Hard Sciences">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_biology(Text) ->
    Keywords = [<<"evolution">>, <<"natural selection">>, <<"adaptation">>, <<"ecosystem">>,
                <<"organism">>, <<"species">>, <<"mutation">>, <<"gene">>, <<"cell">>,
                <<"survival">>, <<"reproduction">>, <<"fitness">>, <<"niche">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    BaseScore = KeywordCount * 0.06,
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Biology">>,
        <<"category">> => <<"Hard Sciences">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount},
        <<"confidence">> => confidence_level(Score)
    }.

%%====================================================================
%% Detection Algorithms - Psychology & Biases
%%====================================================================

detect_psychology(Text) ->
    Keywords = [<<"behavior">>, <<"cognitive">>, <<"emotion">>, <<"motivation">>,
                <<"perception">>, <<"memory">>, <<"learning">>, <<"bias">>,
                <<"heuristic">>, <<"decision">>, <<"judgment">>, <<"thinking">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    BaseScore = KeywordCount * 0.05,
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Psychology">>,
        <<"category">> => <<"Psychology: Biases">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_confirmation_bias(Text) ->
    Keywords = [<<"confirm">>, <<"belief">>, <<"evidence">>, <<"support">>,
                <<"agree">>, <<"validate">>, <<"ignore">>, <<"contrary">>,
                <<"opposite">>, <<"proof">>, <<"convinced">>, <<"certain">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    %% Check for negative patterns
    PatternCount = count_patterns(TextLower, [
        <<"only look">>, <<"only see">>, <<"ignore evidence">>,
        <<"dismiss">>, <<"overlook">>, <<"always right">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Confirmation Bias">>,
        <<"category">> => <<"Psychology: Biases">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_anchoring_bias(Text) ->
    Keywords = [<<"first">>, <<"initial">>, <<"starting">>, <<"reference">>,
                <<"number">>, <<"price">>, <<"estimate">>, <<"adjust">>,
                <<"anchor">>, <<"fixed">>, <<"baseline">>, <<"comparison">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"first number">>, <<"starting point">>, <<"starting price">>,
        <<"anchored to">>, <<"compared to">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Anchoring">>,
        <<"category">> => <<"Psychology: Biases">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_availability_heuristic(Text) ->
    Keywords = [<<"recent">>, <<"memorable">>, <<"vivid">>, <<"salient">>,
                <<"common">>, <<"frequent">>, <<"recall">>, <<"remember">>,
                <<"example">>, <<"instance">>, <<"case">>, <<"heard">>,
                <<"seen">>, <<"read">>, <<"news">>, <<"media">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"recently heard">>, <<"just saw">>, <<"in the news">>,
        <<"everyone knows">>, <<"memorable example">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Availability Heuristic">>,
        <<"category">> => <<"Psychology: Biases">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_overconfidence_bias(Text) ->
    Keywords = [<<"certain">>, <<"sure">>, <<"confident">>, <<"know">>,
                <<"definitely">>, <<"absolutely">>, <<"guarantee">>,
                <<"impossible">>, <<"always">>, <<"never">>, <<"proven">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"absolutely will">>, <<"definitely will">>, <<"certainly will">>,
        <<"impossible to">>, <<"guaranteed to">>, <<"always happen">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Overconfidence">>,
        <<"category">> => <<"Psychology: Biases">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_loss_aversion(Text) ->
    Keywords = [<<"loss">>, <<"lose">>, <<"losing">>, <<"lost">>,
                <<"risk">>, <<"downside">>, <<"protect">>, <<"preserve">>,
                <<"avoid">>, <<"fear">>, <<"worry">>, <<"safe">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"fear of losing">>, <<"avoid loss">>, <<"protect from">>,
        <<"can't afford to lose">>, <<"too risky">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Loss Aversion">>,
        <<"category">> => <<"Psychology: Biases">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_sunk_cost_fallacy(Text) ->
    Keywords = [<<"invested">>, <<"spent">>, <<"already">>, <<"committed">>,
                <<"too much">>, <<"can't stop">>, <<"come this far">>,
                <<"wasted">>, <<"effort">>, <<"time">>, <<"money">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"already invested">>, <<"too much time">>, <<"can't give up now">>,
        <<"come too far">>, <<"wasted effort">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Sunk Cost Fallacy">>,
        <<"category">> => <<"Psychology: Biases">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

%%====================================================================
%% Detection Algorithms - Economics & Business
%%====================================================================

detect_economics(Text) ->
    Keywords = [<<"market">>, <<"price">>, <<"supply">>, <<"demand">>,
                <<"cost">>, <<"profit">>, <<"value">>, <<"trade">>,
                <<"incentive">>, <<"competition">>, <<"monopoly">>, <<"scarcity">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    BaseScore = KeywordCount * 0.05,
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Economics">>,
        <<"category">> => <<"Economics & Finance">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_porters_five_forces(Text) ->
    Keywords = [<<"supplier">>, <<"buyer">>, <<"competitor">>, <<"threat">>,
                <<"bargaining power">>, <<"substitute">>, <<"entry">>,
                <<"exit">>, <<"industry">>, <<"competitive">>, <<"rivalry">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"five forces">>, <<"supplier power">>, <<"buyer power">>,
        <<"threat of">>, <<"competitive advantage">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Porter's Five Forces">>,
        <<"category">> => <<"Economics & Finance">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_economic_moat(Text) ->
    Keywords = [<<"moat">>, <<"defensible">>, <<"competitive advantage">>,
                <<"barrier">>, <<"protection">>, <<"brand">>, <<"network">>,
                <<"switching cost">>, <<"scale">>, <<"cost advantage">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"economic moat">>, <<"competitive barrier">>, <<"switching cost">>,
        <<"network effect">>, <<"brand loyalty">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Economic Moat">>,
        <<"category">> => <<"Economics & Finance">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_margin_of_safety(Text) ->
    Keywords = [<<"safety">>, <<"buffer">>, <<"margin">>, <<"conservative">>,
                <<"cushion">>, <<"protection">>, <<"downside">>, <<"risk">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"margin of safety">>, <<"safety buffer">>, <<"conservative estimate">>,
        <<"downside protection">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Margin of Safety">>,
        <<"category">> => <<"Economics & Finance">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_compound_interest(Text) ->
    Keywords = [<<"compound">>, <<"exponential">>, <<"growth">>, <<"time">>,
                <<"accumulate">>, <<"interest">>, <<"reinvest">>, <<"snowball">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"compound interest">>, <<"exponential growth">>, <<"over time">>,
        <<"snowball effect">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Compound Interest">>,
        <<"category">> => <<"Economics & Finance">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_network_effects(Text) ->
    Keywords = [<<"network">>, <<"effect">>, <<"users">>, <<"platform">>,
                <<"viral">>, <<"adoption">>, <<"critical mass">>, <<"ecosystem">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"network effect">>, <<"more users">>, <<"platform">>,
        <<"viral growth">>, <<"critical mass">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Network Effects">>,
        <<"category">> => <<"Economics & Finance">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

%%====================================================================
%% Detection Algorithms - Thinking Tools
%%====================================================================

detect_inversion(Text) ->
    Keywords = [<<"opposite">>, <<"reverse">>, <<"invert">>, <<"backward">>,
                <<"contrary">>, <<"avoid">>, <<"prevent">>, <<"instead">>,
                <<"flip">>, <<"inverse">>, <<"don't">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"instead of">>, <<"rather than">>, <<"opposite of">>,
        <<"how not to">>, <<"how to avoid">>, <<"inversion">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Inversion">>,
        <<"category">> => <<"Thinking Tools">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_first_principles(Text) ->
    Keywords = [<<"fundamental">>, <<"basic">>, <<"truth">>, <<"reasoning">>,
                <<"axiom">>, <<"assumption">>, <<"ground up">>, <<"scratch">>,
                <<"core">>, <<"essence">>, <<"underlying">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"first principles">>, <<"from scratch">>, <<"ground up">>,
        <<"fundamental truth">>, <<"basic assumption">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"First Principles Thinking">>,
        <<"category">> => <<"Thinking Tools">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

detect_second_order_thinking(Text) ->
    Keywords = [<<"consequences">>, <<"effects">>, <<"long-term">>, <<"ripple">>,
                <<"cascade">>, <<"downstream">>, <<"implications">>, <<"then what">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"second order">>, <<"and then">>, <<"what happens next">>,
        <<"long term">>, <<"downstream effect">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Second-Order Thinking">>,
        <<"category">> => <<"Thinking Tools">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%% Count occurrences of keywords in text
count_keywords(Text, Keywords) ->
    lists:foldl(fun(Keyword, Acc) ->
        case binary:match(Text, Keyword) of
            nomatch -> Acc;
            _ -> Acc + 1
        end
    end, 0, Keywords).

%% Count occurrences of patterns in text
count_patterns(Text, Patterns) ->
    lists:foldl(fun(Pattern, Acc) ->
        case binary:match(Text, Pattern) of
            nomatch -> Acc;
            _ -> Acc + 1
        end
    end, 0, Patterns).

%% Count numbers in text
count_numbers(Text) ->
    %% Simple count of digit sequences
    Matches = binary:matches(Text, [<<"0">>, <<"1">>, <<"2">>, <<"3">>, <<"4">>,
                                     <<"5">>, <<"6">>, <<"7">>, <<"8">>, <<"9">>]),
    length(Matches).

%% Determine confidence level from score
confidence_level(Score) when Score >= 0.7 -> <<"High">>;
confidence_level(Score) when Score >= 0.4 -> <<"Moderate">>;
confidence_level(_) -> <<"Low">>.
