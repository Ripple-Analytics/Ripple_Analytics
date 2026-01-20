%%%-------------------------------------------------------------------
%%% @doc improver_worker Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(improver_worker_part2).

-export([run_improvement_scan/1, generate_improvement/2, deploy_improvements/2, validate_and_deploy/2, validate_improvement/2, generate_id/0]).

run_improvement_scan(State) ->
    BasePath = "/repo",
    
    case code_analyzer:scan_for_improvements(BasePath) of
        {ok, Opportunities} when length(Opportunities) > 0 ->
            io:format("[IMPROVER] Found ~p improvement opportunities~n", [length(Opportunities)]),
            
            BatchSize = application:get_env(code_improver_service, improvement_batch_size, 5),
            TopOpportunities = lists:sublist(Opportunities, BatchSize),
            
            NewImprovements = lists:filtermap(
                fun(Opportunity) ->
                    case generate_improvement(State#state.lm_studio_url, Opportunity) of
                        {ok, Improvement} ->
                            io:format("[IMPROVER] Generated improvement for ~s~n", 
                                     [maps:get(file_path, Opportunity)]),
                            {true, Improvement};
                        {error, Reason} ->
                            io:format("[IMPROVER] Failed to generate improvement: ~p~n", [Reason]),
                            false
                    end
                end,
                TopOpportunities
            ),
            
            case State#state.auto_deploy of
                true ->
                    deploy_improvements(NewImprovements, State);
                false ->
                    io:format("[IMPROVER] Auto-deploy disabled, improvements queued for review~n"),
                    State#state{
                        pending_improvements = State#state.pending_improvements ++ NewImprovements,
                        improvement_count = State#state.improvement_count + length(NewImprovements)
                    }
            end;
        {ok, []} ->
            io:format("[IMPROVER] No improvement opportunities found~n"),
            State;
        {error, Reason} ->
            io:format("[IMPROVER] Error scanning for improvements: ~p~n", [Reason]),
            State
    end.

generate_improvement(LmStudioUrl, Opportunity) ->
    FilePath = maps:get(file_path, Opportunity),
    Content = maps:get(content, Opportunity),
    CodeSmells = maps:get(code_smells, Opportunity, []),
    
    SystemPrompt = munger_prompts:get_system_prompt(),
    UserPrompt = munger_prompts:get_code_review_prompt(binary_to_list(Content)),
    
    case lm_studio_client:generate_improvement(LmStudioUrl, SystemPrompt, UserPrompt) of
        {ok, Response} ->
            ImprovementId = generate_id(),
            Improvement = #{
                id => ImprovementId,
                file_path => FilePath,
                original_content => Content,
                code_smells => CodeSmells,
                analysis => maps:get(<<"analysis">>, Response, <<>>),
                suggested_improvements => maps:get(<<"improvements">>, Response, []),
                improved_code => maps:get(<<"code">>, Response, <<>>),
                risks => maps:get(<<"risks">>, Response, []),
                confidence => maps:get(<<"confidence">>, Response, 0.0),
                status => pending,
                created_at => erlang:system_time(second),
                raw_response => Response
            },
            {ok, Improvement};
        {error, Reason} ->
            {error, Reason}
    end.

deploy_improvements(Improvements, State) ->
    lists:foldl(
        fun(Improvement, AccState) ->
            case validate_and_deploy(Improvement, AccState) of
                {ok, deployed} ->
                    io:format("[IMPROVER] Successfully deployed improvement ~s~n", 
                             [maps:get(id, Improvement)]),
                    AccState#state{
                        deployed_improvements = [Improvement#{status => deployed} | AccState#state.deployed_improvements],
                        improvement_count = AccState#state.improvement_count + 1
                    };
                {error, Reason} ->
                    io:format("[IMPROVER] Failed to deploy improvement: ~p~n", [Reason]),
                    AccState#state{
                        failed_improvements = [Improvement#{status => failed, error => Reason} | AccState#state.failed_improvements]
                    }
            end
        end,
        State,
        Improvements
    ).

validate_and_deploy(Improvement, State) ->
    Confidence = maps:get(confidence, Improvement, 0.0),
    
    case Confidence >= 0.7 of
        true ->
            case validate_improvement(Improvement, State) of
                {ok, valid} ->
                    improvement_deployer:deploy(Improvement);
                {error, Reason} ->
                    {error, {validation_failed, Reason}}
            end;
        false ->
            io:format("[IMPROVER] Confidence too low (~p), skipping deployment~n", [Confidence]),
            {error, low_confidence}
    end.

validate_improvement(Improvement, State) ->
    OriginalCode = maps:get(original_content, Improvement),
    ImprovedCode = maps:get(improved_code, Improvement),
    
    case ImprovedCode of
        <<>> ->
            {error, no_improved_code};
        _ ->
            ValidationPrompt = munger_prompts:get_improvement_validation_prompt(
                binary_to_list(OriginalCode),
                binary_to_list(ImprovedCode)
            ),
            SystemPrompt = munger_prompts:get_system_prompt(),
            
            case lm_studio_client:generate_improvement(State#state.lm_studio_url, SystemPrompt, ValidationPrompt) of
                {ok, Response} ->
                    Valid = maps:get(<<"valid">>, Response, false),
                    Recommendation = maps:get(<<"recommendation">>, Response, <<"review">>),
                    case Valid andalso Recommendation == <<"deploy">> of
                        true -> {ok, valid};
                        false -> {error, {validation_rejected, Response}}
                    end;
                {error, Reason} ->
                    {error, {validation_error, Reason}}
            end
    end.

generate_id() ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(999999),
    list_to_binary(io_lib:format("imp-~p-~p", [Timestamp, Random])).

