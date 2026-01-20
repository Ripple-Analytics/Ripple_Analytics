%%%-------------------------------------------------------------------
%%% @doc autonomous_engine Helper Module - Part 3
%%% @end
%%%-------------------------------------------------------------------
-module(autonomous_engine_part3).

-export([count_long_functions/1, estimate_complexity/1, extract_opportunities/1, conduct_research/2, generate_research_topics/2, do_research/2, build_research_prompt/1, parse_research_response/1, generate_improvements/2]).

count_long_functions(Content) ->
    %% Simple heuristic: count functions > 50 lines
    %% This is approximate but useful for prioritization
    Functions = binary:split(Content, <<".\n">>, [global]),
    length([F || F <- Functions, byte_size(F) > 2000]).

estimate_complexity(Content) ->
    %% Simple complexity score based on control structures
    CaseCount = length(binary:matches(Content, <<"case">>)),
    IfCount = length(binary:matches(Content, <<"if">>)),
    TryCount = length(binary:matches(Content, <<"try">>)),
    CaseCount + IfCount + TryCount.

extract_opportunities(Analysis) ->
    lists:filtermap(fun(A) ->
        case has_issues(A) of
            true -> {true, maps:get(<<"file">>, A, <<>>)};
            false -> false
        end
    end, Analysis).

%%====================================================================
%% Phase 2: Self-Directed Research
%%====================================================================

conduct_research(State, AnalysisResult) ->
    case State#state.research_queue of
        [] ->
            %% Generate new research topics based on analysis
            NewTopics = generate_research_topics(AnalysisResult, State#state.knowledge_base),
            State#state{research_queue = NewTopics};
        [Topic | Rest] ->
            %% Conduct research on the topic
            case do_research(Topic, State#state.knowledge_base) of
                {ok, Findings} ->
                    %% Add findings to knowledge base
                    TopicKey = maps:get(topic, Topic),
                    NewKB = maps:put(TopicKey, Findings, State#state.knowledge_base),
                    State#state{
                        research_queue = Rest,
                        completed_research = [#{topic => Topic, findings => Findings} | 
                                              State#state.completed_research],
                        knowledge_base = NewKB,
                        research_completed = State#state.research_completed + 1
                    };
                {error, _Reason} ->
                    %% Move to end of queue and try later
                    State#state{research_queue = Rest ++ [Topic]}
            end
    end.

generate_research_topics(AnalysisResult, KnowledgeBase) ->
    %% Identify gaps in knowledge based on codebase analysis
    Opportunities = maps:get(<<"improvement_opportunities">>, AnalysisResult, []),
    ExistingTopics = maps:keys(KnowledgeBase),
    
    %% Generate topics for areas we don't know about yet
    BaseTopics = [
        #{topic => <<"error_handling_patterns">>, priority => high},
        #{topic => <<"gen_server_optimization">>, priority => medium},
        #{topic => <<"cowboy_handler_patterns">>, priority => medium},
        #{topic => <<"ets_best_practices">>, priority => low},
        #{topic => <<"supervision_tree_design">>, priority => high}
    ],
    
    %% Filter out topics we already know
    [T || T <- BaseTopics, not lists:member(maps:get(topic, T), ExistingTopics)].

do_research(Topic, _ExistingKnowledge) ->
    TopicName = maps:get(topic, Topic),
    io:format("[AUTONOMOUS] Researching: ~s~n", [TopicName]),
    
    %% Build research prompt
    Prompt = build_research_prompt(TopicName),
    SystemPrompt = "You are an expert Erlang/OTP developer and system architect. "
                   "Provide detailed, actionable knowledge about the requested topic. "
                   "Focus on best practices, common patterns, and practical implementation advice. "
                   "Format your response as JSON with keys: summary, best_practices (list), "
                   "common_patterns (list), pitfalls_to_avoid (list), code_examples (list).",
    
    case lm_client:generate(Prompt, SystemPrompt) of
        {ok, Response} ->
            %% Parse the research findings
            parse_research_response(Response);
        {error, Reason} ->
            {error, Reason}
    end.

build_research_prompt(TopicName) ->
    "Please provide comprehensive knowledge about: " ++ binary_to_list(TopicName) ++ "\n\n"
    "Context: This is for an Erlang/OTP microservices system with:\n"
    "- Cowboy HTTP handlers\n"
    "- gen_server processes\n"
    "- Blue-green deployments\n"
    "- Docker containers\n"
    "- Self-healing capabilities\n\n"
    "Provide practical, implementable advice.".

parse_research_response(Response) ->
    try
        Decoded = jsx:decode(Response, [return_maps]),
        {ok, Decoded}
    catch
        _:_ ->
            %% If not valid JSON, wrap the response
            {ok, #{
                <<"summary">> => Response,
                <<"best_practices">> => [],
                <<"common_patterns">> => [],
                <<"pitfalls_to_avoid">> => [],
                <<"code_examples">> => []
            }}
    end.

%%====================================================================
%% Phase 3: Generate Improvements
%%====================================================================

generate_improvements(State, AnalysisResult) ->
    Opportunities = maps:get(<<"improvement_opportunities">>, AnalysisResult, []),
    
    %% Take top N opportunities
    TargetFiles = lists:sublist(Opportunities, ?MAX_IMPROVEMENTS_PER_CYCLE),
    
    %% Generate improvements for each
    Improvements = lists:filtermap(fun(FilePath) ->
        case generate_improvement_for_file(FilePath, State#state.knowledge_base) of
            {ok, Improvement} -> {true, Improvement};
            {error, _} -> false
        end
    end, TargetFiles),
    
    State#state{improvement_queue = Improvements}.

