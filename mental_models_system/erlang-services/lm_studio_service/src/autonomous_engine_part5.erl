%%%-------------------------------------------------------------------
%%% @doc autonomous_engine Helper Module - Part 5
%%% @end
%%%-------------------------------------------------------------------
-module(autonomous_engine_part5).

-export([validate_code_compiles/1, deploy_improvement/1, learn_from_cycle/1, extract_success_patterns/1, extract_failure_patterns/1, load_knowledge_base/0, save_knowledge_base/1, ensure_timer_running/1, ensure_timer_running/1]).

validate_code_compiles(Improvement) ->
    NewCode = maps:get(<<"new_code">>, Improvement, <<>>),
    
    %% Write to temp file and try to compile
    TempFile = "/tmp/validate_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".erl",
    
    %% We need a complete module to compile, so wrap the code
    TestModule = <<"-module(validate_temp).\n", NewCode/binary>>,
    
    case file:write_file(TempFile, TestModule) of
        ok ->
            Result = os:cmd("erlc -W " ++ TempFile ++ " 2>&1"),
            file:delete(TempFile),
            file:delete("/tmp/validate_temp.beam"),
            
            case string:find(Result, "error") of
                nomatch -> {ok, validated};
                _ -> {error, {compile_error, Result}}
            end;
        {error, Reason} ->
            {error, {write_failed, Reason}}
    end.

deploy_improvement(Improvement) ->
    FilePath = binary_to_list(maps:get(<<"file">>, Improvement)),
    OldCode = maps:get(<<"old_code">>, Improvement),
    NewCode = maps:get(<<"new_code">>, Improvement),
    
    case file:read_file(FilePath) of
        {ok, Content} ->
            case binary:match(Content, OldCode) of
                nomatch ->
                    {error, old_code_not_found};
                _ ->
                    UpdatedContent = binary:replace(Content, OldCode, NewCode),
                    case file:write_file(FilePath, UpdatedContent) of
                        ok ->
                            io:format("[AUTONOMOUS] Deployed improvement to ~s~n", [FilePath]),
                            {ok, deployed};
                        {error, Reason} ->
                            {error, {write_failed, Reason}}
                    end
            end;
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.

%%====================================================================
%% Phase 5: Learning
%%====================================================================

learn_from_cycle(State) ->
    %% Analyze what worked and what didn't
    SuccessPatterns = extract_success_patterns(State#state.completed_improvements),
    FailurePatterns = extract_failure_patterns(State#state.failed_improvements),
    
    %% Update learned patterns
    NewPatterns = SuccessPatterns ++ FailurePatterns,
    
    State#state{
        learned_patterns = NewPatterns ++ State#state.learned_patterns
    }.

extract_success_patterns(Improvements) ->
    [#{
        type => success,
        file_type => determine_file_type(maps:get(<<"file">>, I, <<>>)),
        description => maps:get(<<"description">>, I, <<>>)
    } || I <- Improvements].

extract_failure_patterns(Failures) ->
    [#{
        type => failure,
        reason => Reason
    } || {_Improvement, Reason} <- Failures].

%%====================================================================
%% Knowledge Base Persistence
%%====================================================================

load_knowledge_base() ->
    case file:read_file(?KNOWLEDGE_FILE) of
        {ok, Content} ->
            try jsx:decode(Content, [return_maps])
            catch _:_ -> #{}
            end;
        {error, _} ->
            #{}
    end.

save_knowledge_base(KnowledgeBase) ->
    %% Ensure directory exists
    filelib:ensure_dir(?KNOWLEDGE_FILE),
    Content = jsx:encode(KnowledgeBase, [pretty]),
    file:write_file(?KNOWLEDGE_FILE, Content).

%%====================================================================
%% Utilities
%%====================================================================

ensure_timer_running(undefined) ->
    erlang:send_after(?CYCLE_INTERVAL, self(), run_cycle);
ensure_timer_running(Ref) ->
    case erlang:read_timer(Ref) of
        false -> erlang:send_after(?CYCLE_INTERVAL, self(), run_cycle);
        _ -> Ref
    end.

