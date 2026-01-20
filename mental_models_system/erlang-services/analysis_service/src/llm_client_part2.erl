%%%-------------------------------------------------------------------
%%% @doc llm_client Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(llm_client_part2).

-export([llm_analyze/3, llm_detect_biases/2, call_llm_with_retry/3, call_llm_with_retry/3]).

llm_analyze(Url, Text, TopN) ->
    SystemPrompt = <<"You are an expert analyst trained in mental models, cognitive frameworks, and decision-making tools. You have deep knowledge of:

THINKING TOOLS: First Principles, Inversion, Circle of Competence, Second-Order Thinking, Occam's Razor, Hanlon's Razor, Probabilistic Thinking

PSYCHOLOGY & BIASES: Confirmation Bias, Loss Aversion, Availability Heuristic, Anchoring, Hindsight Bias, Sunk Cost Fallacy, Dunning-Kruger Effect, Social Proof, Recency Bias, Status Quo Bias

ECONOMICS & FINANCE: Margin of Safety, Compound Interest, Opportunity Cost, Supply & Demand, Incentives, Comparative Advantage, Diminishing Returns, Network Effects

SYSTEMS THINKING: Feedback Loops, Emergence, Bottlenecks, Leverage Points, Redundancy

PHYSICS MODELS: Entropy, Inertia, Critical Mass, Activation Energy

BIOLOGY MODELS: Natural Selection, Adaptation, Red Queen Effect, Ecosystems

MATHEMATICS: Regression to Mean, Power Laws, Bayes' Theorem, Law of Large Numbers

ORGANIZATIONAL: Economies of Scale, Switching Costs, Brand Moats, Principal-Agent Problem, Bureaucracy

Analyze text to identify which mental models are most relevant and explain how they apply.">>,

    Prompt = <<"Analyze the following text and identify the top ", 
               (integer_to_binary(TopN))/binary, 
               " most relevant mental models that apply:\n\n",
               Text/binary,
               "\n\nFor each mental model, provide a JSON object with:\n",
               "- \"name\": The model name\n",
               "- \"category\": The category (Thinking Tools, Psychology, Economics, etc.)\n",
               "- \"relevance\": Score from 0-100\n",
               "- \"explanation\": How this model applies to the text\n",
               "- \"actionable_insight\": What action or decision this suggests\n\n",
               "Return a JSON array of objects. Be specific and practical.">>,
    
    RequestBody = jsx:encode(#{
        <<"model">> => <<"local-model">>,
        <<"messages">> => [
            #{<<"role">> => <<"system">>, 
              <<"content">> => SystemPrompt},
            #{<<"role">> => <<"user">>, 
              <<"content">> => Prompt}
        ],
        <<"max_tokens">> => 1500,
        <<"temperature">> => 0.7
    }),
    
    case call_llm_with_retry(Url, RequestBody, ?MAX_RETRIES) of
        {ok, Response} ->
            {ok, #{
                <<"success">> => true,
                <<"analysis">> => Response,
                <<"llm_powered">> => true
            }};
        {error, Reason} ->
            %% Fallback to keyword analysis
            FallbackResult = keyword_analyze(Text, TopN),
            maps:put(<<"llm_error">>, list_to_binary(io_lib:format("~p", [Reason])), FallbackResult)
    end.

llm_detect_biases(Url, Text) ->
    SystemPrompt = <<"You are an expert in cognitive biases, behavioral psychology, and decision-making errors. You can identify subtle patterns of biased thinking in text, including:

JUDGMENT BIASES:
- Confirmation Bias: Seeking information that confirms existing beliefs
- Anchoring: Over-relying on first piece of information
- Availability Heuristic: Overweighting easily recalled examples
- Hindsight Bias: Believing past events were predictable

DECISION BIASES:
- Loss Aversion: Preferring to avoid losses over acquiring gains
- Sunk Cost Fallacy: Continuing due to past investment
- Status Quo Bias: Preference for current state of affairs
- Endowment Effect: Overvaluing what we own

SOCIAL BIASES:
- Social Proof: Following the crowd
- Authority Bias: Over-trusting authority figures
- In-group Bias: Favoring one's own group
- Halo Effect: Letting one trait influence overall judgment

SELF-ASSESSMENT BIASES:
- Dunning-Kruger Effect: Overestimating competence when unskilled
- Overconfidence: Excessive certainty in own answers
- Self-Serving Bias: Attributing success to self, failure to others

Analyze text carefully for evidence of these biases.">>,

    Prompt = <<"Analyze the following text for cognitive biases:\n\n",
               Text/binary,
               "\n\nFor each bias detected, provide a JSON object with:\n",
               "- \"bias\": The bias name (snake_case)\n",
               "- \"category\": Judgment, Decision, Social, or Self-Assessment\n",
               "- \"severity\": low, medium, or high\n",
               "- \"evidence\": Array of specific phrases/patterns that indicate this bias\n",
               "- \"explanation\": Why this indicates the bias\n",
               "- \"mitigation\": How to counteract this bias\n\n",
               "Return a JSON array. Only include biases with clear evidence.">>,
    
    RequestBody = jsx:encode(#{
        <<"model">> => <<"local-model">>,
        <<"messages">> => [
            #{<<"role">> => <<"system">>, 
              <<"content">> => SystemPrompt},
            #{<<"role">> => <<"user">>, 
              <<"content">> => Prompt}
        ],
        <<"max_tokens">> => 1500,
        <<"temperature">> => 0.7
    }),
    
    case call_llm_with_retry(Url, RequestBody, ?MAX_RETRIES) of
        {ok, Response} ->
            {ok, #{
                <<"success">> => true,
                <<"biases">> => Response,
                <<"llm_powered">> => true
            }};
        {error, Reason} ->
            FallbackResult = keyword_detect_biases(Text),
            maps:put(<<"llm_error">>, list_to_binary(io_lib:format("~p", [Reason])), FallbackResult)
    end.

call_llm_with_retry(_Url, _Body, 0) ->
    {error, max_retries_exceeded};
call_llm_with_retry(Url, Body, Retries) ->
    ApiUrl = Url ++ "/v1/chat/completions",
    Headers = [{<<"content-type">>, <<"application/json">>}],
    
    case hackney:request(post, list_to_binary(ApiUrl), Headers, Body, [{timeout, ?TIMEOUT}]) of
        {ok, 200, _, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            Response = jsx:decode(RespBody, [return_maps]),
            Content = maps:get(<<"content">>, 
                        maps:get(<<"message">>, 
                            hd(maps:get(<<"choices">>, Response)))),
            {ok, Content};
        {ok, Status, _, _} ->
            timer:sleep(1000 * (4 - Retries)),
            call_llm_with_retry(Url, Body, Retries - 1);
        {error, _Reason} ->
            timer:sleep(1000 * (4 - Retries)),
            call_llm_with_retry(Url, Body, Retries - 1)
    end.

