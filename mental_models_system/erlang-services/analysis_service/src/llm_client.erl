%%%-------------------------------------------------------------------
%%% @doc LLM Client
%%% 
%%% Handles integration with LM Studio for AI-powered analysis.
%%% Includes retry logic and fallback to keyword-based analysis.
%%% @end
%%%-------------------------------------------------------------------
-module(llm_client).
-behaviour(gen_server).

-export([start_link/0, analyze/2, detect_biases/1, is_available/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 60000).
-define(MAX_RETRIES, 3).

-record(state, {
    url :: string(),
    available :: boolean()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

analyze(Text, TopN) ->
    gen_server:call(?SERVER, {analyze, Text, TopN}, ?TIMEOUT).

detect_biases(Text) ->
    gen_server:call(?SERVER, {detect_biases, Text}, ?TIMEOUT).

is_available() ->
    gen_server:call(?SERVER, is_available).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Url = application:get_env(analysis_service, lm_studio_url, "http://host.docker.internal:1234"),
    
    %% Check availability on startup
    Available = check_availability(Url),
    
    %% Schedule periodic availability checks
    erlang:send_after(30000, self(), check_availability),
    
    {ok, #state{url = Url, available = Available}}.

handle_call({analyze, Text, TopN}, _From, #state{url = Url, available = Available} = State) ->
    Result = case Available of
        true ->
            llm_analyze(Url, Text, TopN);
        false ->
            keyword_analyze(Text, TopN)
    end,
    {reply, Result, State};

handle_call({detect_biases, Text}, _From, #state{url = Url, available = Available} = State) ->
    Result = case Available of
        true ->
            llm_detect_biases(Url, Text);
        false ->
            keyword_detect_biases(Text)
    end,
    {reply, Result, State};

handle_call(is_available, _From, #state{available = Available} = State) ->
    {reply, Available, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_availability, #state{url = Url} = State) ->
    Available = check_availability(Url),
    erlang:send_after(30000, self(), check_availability),
    {noreply, State#state{available = Available}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

check_availability(Url) ->
    HealthUrl = Url ++ "/v1/models",
    case hackney:request(get, list_to_binary(HealthUrl), [], <<>>, [{timeout, 5000}]) of
        {ok, 200, _, _} -> true;
        _ -> false
    end.

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

keyword_analyze(Text, TopN) ->
    Models = model_registry:get_all_models(),
    TextLower = string:lowercase(binary_to_list(Text)),
    
    %% Score each model based on keyword matches
    Scored = lists:map(fun(Model) ->
        Keywords = element(9, Model), %% #model.keywords
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
            <<"name">> => element(3, Model), %% #model.name
            <<"category">> => element(4, Model), %% #model.category
            <<"relevance">> => Score,
            <<"description">> => element(5, Model) %% #model.description
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
