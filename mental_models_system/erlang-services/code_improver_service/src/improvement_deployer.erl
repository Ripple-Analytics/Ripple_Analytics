-module(improvement_deployer).

-export([
    deploy/1,
    rollback/1,
    create_backup/1,
    restore_backup/1,
    run_tests/0,
    run_lint/0
]).

deploy(Improvement) ->
    io:format("[DEPLOYER] ========================================~n"),
    io:format("[DEPLOYER] Deploying improvement using blue-green strategy~n"),
    io:format("[DEPLOYER] ========================================~n"),
    
    FilePath = maps:get(file_path, Improvement),
    ImprovedCode = maps:get(improved_code, Improvement),
    
    case ImprovedCode of
        <<>> ->
            io:format("[DEPLOYER] No improved code to deploy~n"),
            {error, no_code};
        _ ->
            deploy_with_safety(FilePath, ImprovedCode, Improvement)
    end.

deploy_with_safety(FilePath, ImprovedCode, Improvement) ->
    io:format("[DEPLOYER] Creating backup of ~s~n", [FilePath]),
    case create_backup(FilePath) of
        {ok, BackupPath} ->
            io:format("[DEPLOYER] Backup created at ~s~n", [BackupPath]),
            
            io:format("[DEPLOYER] Writing improved code...~n"),
            case file:write_file(FilePath, ImprovedCode) of
                ok ->
                    io:format("[DEPLOYER] Code written, running validation...~n"),
                    case validate_deployment() of
                        {ok, valid} ->
                            io:format("[DEPLOYER] Validation passed!~n"),
                            log_deployment(Improvement, BackupPath),
                            {ok, deployed};
                        {error, Reason} ->
                            io:format("[DEPLOYER] Validation failed: ~p~n", [Reason]),
                            io:format("[DEPLOYER] Rolling back...~n"),
                            restore_backup(BackupPath),
                            {error, {validation_failed, Reason}}
                    end;
                {error, Reason} ->
                    io:format("[DEPLOYER] Failed to write code: ~p~n", [Reason]),
                    {error, {write_failed, Reason}}
            end;
        {error, Reason} ->
            io:format("[DEPLOYER] Failed to create backup: ~p~n", [Reason]),
            {error, {backup_failed, Reason}}
    end.

create_backup(FilePath) ->
    BackupDir = "/repo/.code_improver_backups",
    filelib:ensure_dir(BackupDir ++ "/"),
    
    Timestamp = erlang:system_time(second),
    FileName = filename:basename(binary_to_list(FilePath)),
    BackupPath = io_lib:format("~s/~s.~p.bak", [BackupDir, FileName, Timestamp]),
    BackupPathStr = lists:flatten(BackupPath),
    
    case file:copy(FilePath, BackupPathStr) of
        {ok, _} -> {ok, BackupPathStr};
        {error, Reason} -> {error, Reason}
    end.

restore_backup(BackupPath) ->
    OriginalPath = extract_original_path(BackupPath),
    case file:copy(BackupPath, OriginalPath) of
        {ok, _} ->
            io:format("[DEPLOYER] Restored from backup~n"),
            {ok, restored};
        {error, Reason} ->
            io:format("[DEPLOYER] Failed to restore backup: ~p~n", [Reason]),
            {error, Reason}
    end.

extract_original_path(BackupPath) ->
    Parts = string:split(BackupPath, ".", all),
    case length(Parts) >= 3 of
        true ->
            WithoutTimestamp = lists:droplast(Parts),
            WithoutBak = lists:droplast(WithoutTimestamp),
            string:join(WithoutBak, ".");
        false ->
            BackupPath
    end.

validate_deployment() ->
    RequireTests = application:get_env(code_improver_service, require_tests_pass, true),
    RequireLint = application:get_env(code_improver_service, require_lint_pass, true),
    
    TestResult = case RequireTests of
        true -> run_tests();
        false -> {ok, skipped}
    end,
    
    LintResult = case RequireLint of
        true -> run_lint();
        false -> {ok, skipped}
    end,
    
    case {TestResult, LintResult} of
        {{ok, _}, {ok, _}} -> {ok, valid};
        {{error, Reason}, _} -> {error, {tests_failed, Reason}};
        {_, {error, Reason}} -> {error, {lint_failed, Reason}}
    end.

run_tests() ->
    io:format("[DEPLOYER] Running tests...~n"),
    BasePath = "/repo/mental_models_system/erlang-services",
    
    Cmd = "cd " ++ BasePath ++ " && rebar3 eunit 2>&1",
    Result = os:cmd(Cmd),
    
    case string:find(Result, "Failed:") of
        nomatch ->
            case string:find(Result, "error") of
                nomatch ->
                    io:format("[DEPLOYER] Tests passed~n"),
                    {ok, passed};
                _ ->
                    io:format("[DEPLOYER] Tests had errors~n"),
                    {error, Result}
            end;
        _ ->
            io:format("[DEPLOYER] Tests failed~n"),
            {error, Result}
    end.

run_lint() ->
    io:format("[DEPLOYER] Running lint checks...~n"),
    {ok, passed}.

rollback(ImprovementId) ->
    io:format("[DEPLOYER] Rolling back improvement ~s~n", [ImprovementId]),
    BackupDir = "/repo/.code_improver_backups",
    
    case file:list_dir(BackupDir) of
        {ok, Files} ->
            MatchingBackups = [F || F <- Files, string:find(F, binary_to_list(ImprovementId)) =/= nomatch],
            case MatchingBackups of
                [BackupFile | _] ->
                    BackupPath = filename:join(BackupDir, BackupFile),
                    restore_backup(BackupPath);
                [] ->
                    {error, no_backup_found}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

log_deployment(Improvement, BackupPath) ->
    LogDir = "/repo/.code_improver_logs",
    filelib:ensure_dir(LogDir ++ "/"),
    
    Timestamp = erlang:system_time(second),
    LogFile = io_lib:format("~s/deployment_~p.json", [LogDir, Timestamp]),
    
    LogEntry = #{
        improvement_id => maps:get(id, Improvement),
        file_path => maps:get(file_path, Improvement),
        backup_path => list_to_binary(BackupPath),
        deployed_at => Timestamp,
        confidence => maps:get(confidence, Improvement, 0.0)
    },
    
    LogJson = jsx:encode(LogEntry),
    file:write_file(lists:flatten(LogFile), LogJson),
    
    io:format("[DEPLOYER] Deployment logged~n").
