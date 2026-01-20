%%%-------------------------------------------------------------------
%%% @doc improvement_scheduler Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(improvement_scheduler_part2).

-export([run_improvement_pipeline/0, generate_improvement/2, parse_improvement_response/2, validate_and_deploy/1, parse_validation_response/1, deploy_improvement/1]).

run_improvement_pipeline() ->
    ServicesPath = "/app/services",
    
    case code_analyzer:analyze_directory(ServicesPath) of
        {ok, AnalysisResults} ->
            FilesWithIssues = lists:filter(fun({_File, Analysis}) ->
                case Analysis of
                    #{<<"issues">> := Issues} when length(Issues) > 0 -> true;
                    _ -> false
                end
            end, AnalysisResults),
            
            MaxChanges = application:get_env(ai_code_improver, max_changes_per_cycle, 3),
            TargetFiles = lists:sublist(FilesWithIssues, MaxChanges),
            
            Improvements = lists:filtermap(fun({FilePath, Analysis}) ->
                case generate_improvement(FilePath, Analysis) of
                    {ok, Improvement} ->
                        case validate_and_deploy(Improvement) of
                            {ok, DeployedImprovement} -> {true, DeployedImprovement};
                            {error, _} -> false
                        end;
                    {error, _} -> false
                end
            end, TargetFiles),
            
            {ok, Improvements};
        {error, Reason} ->
            {error, {analysis_failed, Reason}}
    end.

generate_improvement(FilePath, Analysis) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            CodeContext = io_lib:format("File: ~s~nAnalysis: ~p~nContent:~n~s", 
                                        [FilePath, Analysis, Content]),
            Prompt = design_philosophy:get_improvement_prompt(lists:flatten(CodeContext)),
            SystemPrompt = design_philosophy:get_system_prompt(),
            
            case lm_studio_client:generate(Prompt, SystemPrompt) of
                {ok, Response} ->
                    parse_improvement_response(FilePath, Response);
                {error, Reason} ->
                    {error, {generation_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {file_read_failed, Reason}}
    end.

parse_improvement_response(FilePath, Response) ->
    try
        Decoded = jsx:decode(Response, [return_maps]),
        Improvements = maps:get(<<"improvements">>, Decoded, []),
        case Improvements of
            [First | _] ->
                {ok, #{
                    <<"file">> => list_to_binary(FilePath),
                    <<"description">> => maps:get(<<"description">>, First, <<>>),
                    <<"old_code">> => maps:get(<<"old_code">>, First, <<>>),
                    <<"new_code">> => maps:get(<<"new_code">>, First, <<>>),
                    <<"benefit">> => maps:get(<<"benefit">>, First, <<>>),
                    <<"timestamp">> => erlang:system_time(second)
                }};
            [] ->
                {error, no_improvements_suggested}
        end
    catch
        _:_ ->
            {error, {parse_failed, Response}}
    end.

validate_and_deploy(Improvement) ->
    RequireValidation = application:get_env(ai_code_improver, require_validation, true),
    AutoDeploy = application:get_env(ai_code_improver, auto_deploy, true),
    
    case RequireValidation of
        true ->
            NewCode = maps:get(<<"new_code">>, Improvement, <<>>),
            ValidationPrompt = design_philosophy:get_validation_prompt(binary_to_list(NewCode)),
            case lm_studio_client:generate(ValidationPrompt, "You are a code validator.") of
                {ok, ValidationResponse} ->
                    case parse_validation_response(ValidationResponse) of
                        {ok, true} ->
                            case AutoDeploy of
                                true -> deploy_improvement(Improvement);
                                false -> {ok, Improvement#{<<"status">> => <<"pending_deploy">>}}
                            end;
                        {ok, false} ->
                            {error, validation_failed};
                        {error, Reason} ->
                            {error, {validation_parse_failed, Reason}}
                    end;
                {error, Reason} ->
                    {error, {validation_request_failed, Reason}}
            end;
        false ->
            case AutoDeploy of
                true -> deploy_improvement(Improvement);
                false -> {ok, Improvement#{<<"status">> => <<"pending_deploy">>}}
            end
    end.

parse_validation_response(Response) ->
    try
        Decoded = jsx:decode(Response, [return_maps]),
        Valid = maps:get(<<"valid">>, Decoded, false),
        {ok, Valid}
    catch
        _:_ ->
            {error, parse_failed}
    end.

deploy_improvement(Improvement) ->
    FilePath = binary_to_list(maps:get(<<"file">>, Improvement)),
    OldCode = maps:get(<<"old_code">>, Improvement),
    NewCode = maps:get(<<"new_code">>, Improvement),
    
    case file:read_file(FilePath) of
        {ok, Content} ->
            UpdatedContent = binary:replace(Content, OldCode, NewCode),
            case file:write_file(FilePath, UpdatedContent) of
                ok ->
                    io:format("[AI Improver] Deployed improvement to ~s~n", [FilePath]),
                    {ok, Improvement#{<<"status">> => <<"deployed">>}};
                {error, Reason} ->
                    {error, {write_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.

