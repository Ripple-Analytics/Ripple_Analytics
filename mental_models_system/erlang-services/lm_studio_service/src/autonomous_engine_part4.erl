%%%-------------------------------------------------------------------
%%% @doc autonomous_engine Helper Module - Part 4
%%% @end
%%%-------------------------------------------------------------------
-module(autonomous_engine_part4).

-export([generate_improvement_for_file/2, build_improvement_prompt/3, get_improvement_system_prompt/0, extract_relevant_knowledge/2, determine_file_type/1, format_knowledge/1, format_knowledge/1, parse_improvement_response/2, validate_and_deploy_improvements/1, validate_improvement/1]).

generate_improvement_for_file(FilePath, KnowledgeBase) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            %% Build improvement prompt with knowledge context
            Prompt = build_improvement_prompt(FilePath, Content, KnowledgeBase),
            SystemPrompt = get_improvement_system_prompt(),
            
            case lm_client:generate(Prompt, SystemPrompt) of
                {ok, Response} ->
                    parse_improvement_response(FilePath, Response);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

build_improvement_prompt(FilePath, Content, KnowledgeBase) ->
    %% Include relevant knowledge in the prompt
    RelevantKnowledge = extract_relevant_knowledge(FilePath, KnowledgeBase),
    
    "Analyze this Erlang file and suggest ONE specific improvement.\n\n"
    "File: " ++ binary_to_list(FilePath) ++ "\n\n"
    "Relevant best practices:\n" ++ format_knowledge(RelevantKnowledge) ++ "\n\n"
    "Current code:\n```erlang\n" ++ binary_to_list(Content) ++ "\n```\n\n"
    "Respond with JSON containing:\n"
    "- description: What the improvement does\n"
    "- old_code: The exact code to replace (must match exactly)\n"
    "- new_code: The improved code\n"
    "- benefit: Why this is better\n"
    "- risk_level: low/medium/high".

get_improvement_system_prompt() ->
    "You are an expert Erlang/OTP developer specializing in code improvement. "
    "Your improvements must be:\n"
    "1. Safe - no breaking changes\n"
    "2. Incremental - small, focused changes\n"
    "3. Testable - the code must compile\n"
    "4. Beneficial - clear improvement in quality, performance, or maintainability\n\n"
    "Always respond with valid JSON. The old_code must match EXACTLY what's in the file.".

extract_relevant_knowledge(FilePath, KnowledgeBase) ->
    %% Determine what knowledge is relevant based on file content
    FileType = determine_file_type(FilePath),
    RelevantKeys = case FileType of
        handler -> [<<"cowboy_handler_patterns">>, <<"error_handling_patterns">>];
        gen_server -> [<<"gen_server_optimization">>, <<"supervision_tree_design">>];
        _ -> [<<"erlang_otp_best_practices">>]
    end,
    
    maps:with(RelevantKeys, KnowledgeBase).

determine_file_type(FilePath) ->
    case binary:match(FilePath, <<"_handler.erl">>) of
        nomatch ->
            case binary:match(FilePath, <<"_sup.erl">>) of
                nomatch -> other;
                _ -> supervisor
            end;
        _ -> handler
    end.

format_knowledge(KnowledgeMap) when map_size(KnowledgeMap) =:= 0 ->
    "No specific knowledge available yet.";
format_knowledge(KnowledgeMap) ->
    lists:flatten([
        io_lib:format("~s: ~p~n", [K, V]) 
        || {K, V} <- maps:to_list(KnowledgeMap)
    ]).

parse_improvement_response(FilePath, Response) ->
    try
        Decoded = jsx:decode(Response, [return_maps]),
        {ok, #{
            <<"file">> => FilePath,
            <<"description">> => maps:get(<<"description">>, Decoded, <<>>),
            <<"old_code">> => maps:get(<<"old_code">>, Decoded, <<>>),
            <<"new_code">> => maps:get(<<"new_code">>, Decoded, <<>>),
            <<"benefit">> => maps:get(<<"benefit">>, Decoded, <<>>),
            <<"risk_level">> => maps:get(<<"risk_level">>, Decoded, <<"medium">>),
            <<"timestamp">> => erlang:system_time(second)
        }}
    catch
        _:_ ->
            {error, parse_failed}
    end.

%%====================================================================
%% Phase 4: Validate and Deploy
%%====================================================================

validate_and_deploy_improvements(State) ->
    %% Process each improvement in the queue
    {Deployed, Failed} = lists:foldl(fun(Improvement, {DepAcc, FailAcc}) ->
        case validate_improvement(Improvement) of
            {ok, validated} ->
                case deploy_improvement(Improvement) of
                    {ok, _} -> 
                        {[Improvement | DepAcc], FailAcc};
                    {error, Reason} -> 
                        {DepAcc, [{Improvement, Reason} | FailAcc]}
                end;
            {error, Reason} ->
                {DepAcc, [{Improvement, Reason} | FailAcc]}
        end
    end, {[], []}, State#state.improvement_queue),
    
    State#state{
        improvement_queue = [],
        completed_improvements = Deployed ++ State#state.completed_improvements,
        failed_improvements = Failed ++ State#state.failed_improvements,
        total_improvements = State#state.total_improvements + length(Deployed),
        successful_deploys = State#state.successful_deploys + length(Deployed),
        failed_deploys = State#state.failed_deploys + length(Failed)
    }.

validate_improvement(Improvement) ->
    %% Check risk level
    RiskLevel = maps:get(<<"risk_level">>, Improvement, <<"medium">>),
    case RiskLevel of
        <<"high">> ->
            io:format("[AUTONOMOUS] Skipping high-risk improvement~n"),
            {error, high_risk};
        _ ->
            %% Validate the code compiles
            validate_code_compiles(Improvement)
    end.

