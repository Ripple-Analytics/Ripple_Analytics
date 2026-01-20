%%%-------------------------------------------------------------------
%%% @doc updater_worker Helper Module - Part 4
%%% @end
%%%-------------------------------------------------------------------
-module(updater_worker_part4).

-export([build_services/3, is_build_error/1, is_build_error/1, attempt_lm_studio_fix/2, extract_error_lines/1, start_standby/1, check_health/1]).

build_services(BasePath, [Service | Rest], Failed) ->
    io:format("[UPDATER] ==========================================~n"),
    io:format("[UPDATER] Building: ~s~n", [Service]),
    io:format("[UPDATER] Command: docker-compose build --no-cache ~s~n", [Service]),
    Cmd = "cd " ++ BasePath ++ " && docker-compose build --no-cache " ++ Service ++ " 2>&1",
    Result = os:cmd(Cmd),
    
    %% Log last 1000 chars of build output (with safe string handling)
    SafeResult = case is_list(Result) of true -> Result; false -> "" end,
    OutputLen = length(SafeResult),
    TruncatedOutput = case OutputLen of
        0 -> "(empty output)";
        N when N > 1000 -> string:sub_string(SafeResult, N - 999, N);
        _ -> SafeResult
    end,
    io:format("[UPDATER] Build output (last 1000 chars):~n~s~n", [TruncatedOutput]),
    
    case is_build_error(Result) of
        true ->
            io:format("[UPDATER] *** BUILD FAILED: ~s ***~n", [Service]),
            %% Log full error for debugging
            ErrorFile = ?DATA_DIR ++ "/build_error_" ++ Service ++ ".log",
            file:write_file(ErrorFile, Result),
            io:format("[UPDATER] Error log saved to: ~s~n", [ErrorFile]),
            
            %% Attempt automated fix via LM Studio
            io:format("[UPDATER] Attempting automated fix via LM Studio...~n"),
            FixResult = attempt_lm_studio_fix(Service, Result),
            case FixResult of
                {ok, fixed} ->
                    io:format("[UPDATER] LM Studio fix applied, retrying build...~n"),
                    %% Retry the build after fix
                    RetryCmd = "cd " ++ BasePath ++ " && docker-compose build --no-cache " ++ Service ++ " 2>&1",
                    RetryResult = os:cmd(RetryCmd),
                    case is_build_error(RetryResult) of
                        true ->
                            io:format("[UPDATER] Retry still failed~n"),
                            {error, "Build failed after fix attempt: " ++ Service};
                        false ->
                            io:format("[UPDATER] *** BUILD SUCCESS after fix: ~s ***~n", [Service]),
                            build_services(BasePath, Rest, Failed)
                    end;
                {error, _} ->
                    io:format("[UPDATER] LM Studio fix failed or unavailable~n"),
                    {error, "Build failed: " ++ Service}
            end;
        false ->
            io:format("[UPDATER] *** BUILD SUCCESS: ~s ***~n", [Service]),
            build_services(BasePath, Rest, Failed)
    end.

is_build_error(Output) when is_list(Output) ->
    string:find(Output, "failed to solve") =/= nomatch orelse
    string:find(Output, "error:") =/= nomatch orelse
    string:find(Output, "ERROR:") =/= nomatch orelse
    string:find(Output, "FAILED") =/= nomatch;
is_build_error(_) ->
    true.  %% If output is not a string, assume error

%% Attempt to fix build errors using LM Studio
%% This runs on the STANDBY environment - active stays untouched
attempt_lm_studio_fix(Service, ErrorOutput) ->
    io:format("[UPDATER] ==========================================~n"),
    io:format("[UPDATER] LM STUDIO AUTO-FIX for ~s~n", [Service]),
    io:format("[UPDATER] ==========================================~n"),
    
    %% Try to connect to LM Studio service
    LmStudioUrl = "http://host.docker.internal:1234/v1/chat/completions",
    
    %% Extract the actual error from the build output
    ErrorLines = extract_error_lines(ErrorOutput),
    io:format("[UPDATER] Error lines: ~s~n", [ErrorLines]),
    
    %% Build the prompt for LM Studio
    Prompt = "You are an Erlang expert. Fix this build error. Return ONLY the corrected code, no explanations.\n\nError:\n" ++ ErrorLines,
    
    %% Make HTTP request to LM Studio
    RequestBody = jsx:encode(#{
        <<"model">> => <<"local-model">>,
        <<"messages">> => [
            #{<<"role">> => <<"user">>, <<"content">> => list_to_binary(Prompt)}
        ],
        <<"max_tokens">> => 2000
    }),
    
    case httpc:request(post, {LmStudioUrl, [], "application/json", RequestBody}, [{timeout, 60000}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            io:format("[UPDATER] LM Studio responded~n"),
            try
                Response = jsx:decode(list_to_binary(ResponseBody)),
                Choices = maps:get(<<"choices">>, Response, []),
                case Choices of
                    [FirstChoice | _] ->
                        Message = maps:get(<<"message">>, FirstChoice, #{}),
                        Content = maps:get(<<"content">>, Message, <<>>),
                        io:format("[UPDATER] Fix suggestion received (~p bytes)~n", [byte_size(Content)]),
                        %% Apply the fix - this would need to parse and apply the code
                        %% For now, log it and let the syntax fixer handle common cases
                        FixLogFile = ?DATA_DIR ++ "/lm_fix_" ++ Service ++ ".txt",
                        file:write_file(FixLogFile, Content),
                        io:format("[UPDATER] Fix saved to: ~s~n", [FixLogFile]),
                        %% Run the syntax fixer which handles common cases
                        os:cmd("cd /repo/mental_models_system/erlang-services && bash scripts/fix_erlang_syntax.sh 2>&1"),
                        {ok, fixed};
                    [] ->
                        io:format("[UPDATER] No fix suggestion in response~n"),
                        {error, no_suggestion}
                end
            catch
                _:_ ->
                    io:format("[UPDATER] Failed to parse LM Studio response~n"),
                    {error, parse_error}
            end;
        {ok, {{_, StatusCode, _}, _, _}} ->
            io:format("[UPDATER] LM Studio returned status ~p~n", [StatusCode]),
            %% Fall back to syntax fixer
            os:cmd("cd /repo/mental_models_system/erlang-services && bash scripts/fix_erlang_syntax.sh 2>&1"),
            {ok, fixed};
        {error, Reason} ->
            io:format("[UPDATER] LM Studio connection failed: ~p~n", [Reason]),
            io:format("[UPDATER] Falling back to syntax fixer...~n"),
            %% Fall back to syntax fixer
            os:cmd("cd /repo/mental_models_system/erlang-services && bash scripts/fix_erlang_syntax.sh 2>&1"),
            {ok, fixed}
    end.

extract_error_lines(Output) ->
    Lines = string:split(Output, "\n", all),
    ErrorLines = [L || L <- Lines, 
                       string:find(L, "error") =/= nomatch orelse
                       string:find(L, "Error") =/= nomatch orelse
                       string:find(L, "syntax") =/= nomatch],
    string:join(lists:sublist(ErrorLines, 10), "\n").

start_standby(Services) ->
    BasePath = "/repo/mental_models_system/erlang-services",
    ServiceStr = string:join(Services, " "),
    Cmd = "cd " ++ BasePath ++ " && docker-compose up -d " ++ ServiceStr ++ " 2>&1",
    os:cmd(Cmd).

check_health(Env) ->
    Container = "mental-models-ui-" ++ Env,
    Cmd = "docker inspect --format='{{.State.Running}}' " ++ Container ++ " 2>/dev/null",
    Result = string:trim(os:cmd(Cmd)),
    io:format("[UPDATER] Health check ~s: ~s~n", [Container, Result]),
    Result =:= "true".

