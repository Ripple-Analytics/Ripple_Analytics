%%%-------------------------------------------------------------------
%%% @doc Autonomous Research - Self-directed knowledge acquisition
%%% @end
%%%-------------------------------------------------------------------
-module(autonomous_research).

-export([conduct_research/3, generate_research_topics/2]).

%%====================================================================
%% API
%%====================================================================

conduct_research(ResearchQueue, KnowledgeBase, CompletedCount) ->
    case ResearchQueue of
        [] ->
            {[], KnowledgeBase, CompletedCount, []};
        [Topic | Rest] ->
            case do_research(Topic) of
                {ok, Findings} ->
                    TopicKey = maps:get(topic, Topic),
                    NewKB = maps:put(TopicKey, Findings, KnowledgeBase),
                    Completed = #{topic => Topic, findings => Findings},
                    {Rest, NewKB, CompletedCount + 1, [Completed]};
                {error, _Reason} ->
                    {Rest ++ [Topic], KnowledgeBase, CompletedCount, []}
            end
    end.

generate_research_topics(_AnalysisResult, KnowledgeBase) ->
    ExistingTopics = maps:keys(KnowledgeBase),
    BaseTopics = [
        #{topic => <<"error_handling_patterns">>, priority => high},
        #{topic => <<"gen_server_optimization">>, priority => medium},
        #{topic => <<"cowboy_handler_patterns">>, priority => medium},
        #{topic => <<"ets_best_practices">>, priority => low},
        #{topic => <<"supervision_tree_design">>, priority => high}
    ],
    [T || T <- BaseTopics, not lists:member(maps:get(topic, T), ExistingTopics)].

%%====================================================================
%% Internal
%%====================================================================

do_research(Topic) ->
    TopicName = maps:get(topic, Topic),
    io:format("[AUTONOMOUS] Researching: ~s~n", [TopicName]),
    
    Prompt = build_research_prompt(TopicName),
    SystemPrompt = "You are an expert Erlang/OTP developer. "
                   "Provide detailed, actionable knowledge. "
                   "Format as JSON with: summary, best_practices, "
                   "common_patterns, pitfalls_to_avoid, code_examples.",
    
    case lm_client:generate(Prompt, SystemPrompt) of
        {ok, Response} -> parse_response(Response);
        {error, Reason} -> {error, Reason}
    end.

build_research_prompt(TopicName) ->
    "Provide knowledge about: " ++ binary_to_list(TopicName) ++ "\n\n"
    "Context: Erlang/OTP microservices with Cowboy, gen_server, "
    "blue-green deployments, Docker, self-healing.\n"
    "Provide practical, implementable advice.".

parse_response(Response) ->
    try
        {ok, jsx:decode(Response, [return_maps])}
    catch
        _:_ ->
            {ok, #{
                <<"summary">> => Response,
                <<"best_practices">> => [],
                <<"common_patterns">> => [],
                <<"pitfalls_to_avoid">> => [],
                <<"code_examples">> => []
            }}
    end.
