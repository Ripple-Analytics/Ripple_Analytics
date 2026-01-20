%%%-------------------------------------------------------------------
%%% @doc Autonomous Deployer - Validation and deployment of improvements
%%% @end
%%%-------------------------------------------------------------------
-module(autonomous_deployer).

-export([validate_and_deploy/1, validate_improvement/1, deploy_improvement/1]).

%%====================================================================
%% API
%%====================================================================

validate_and_deploy(ImprovementQueue) ->
    lists:foldl(fun(Improvement, {DepAcc, FailAcc}) ->
        case validate_improvement(Improvement) of
            {ok, validated} ->
                case deploy_improvement(Improvement) of
                    {ok, _} -> {[Improvement | DepAcc], FailAcc};
                    {error, Reason} -> {DepAcc, [{Improvement, Reason} | FailAcc]}
                end;
            {error, Reason} ->
                {DepAcc, [{Improvement, Reason} | FailAcc]}
        end
    end, {[], []}, ImprovementQueue).

validate_improvement(Improvement) ->
    RiskLevel = maps:get(<<"risk_level">>, Improvement, <<"medium">>),
    case RiskLevel of
        <<"high">> ->
            io:format("[AUTONOMOUS] Skipping high-risk improvement~n"),
            {error, high_risk};
        _ ->
            validate_code_compiles(Improvement)
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
                            io:format("[AUTONOMOUS] Deployed to ~s~n", [FilePath]),
                            {ok, deployed};
                        {error, Reason} ->
                            {error, {write_failed, Reason}}
                    end
            end;
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.

%%====================================================================
%% Internal
%%====================================================================

validate_code_compiles(Improvement) ->
    NewCode = maps:get(<<"new_code">>, Improvement, <<>>),
    TempFile = "/tmp/validate_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".erl",
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
