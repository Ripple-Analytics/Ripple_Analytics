%%%-------------------------------------------------------------------
%%% @doc Docker Utilities - Build, start, health check operations
%%% With verbose GitHub logging for remote debugging
%%% @end
%%%-------------------------------------------------------------------
-module(docker_utils).

-export([build_service/2, start_services/2, check_health/1]).
-export([exec_cmd/2, is_build_error/1]).

%% @doc Build a single service with GitHub logging
build_service(BasePath, Service) ->
    io:format("[DOCKER] Building: ~s~n", [Service]),
    Cmd = "cd " ++ BasePath ++ " && docker-compose build --no-cache " ++ Service ++ " 2>&1",
    Result = os:cmd(Cmd),
    log_output(Result),
    
    %% Log to GitHub for remote debugging
    ExitCode = case is_build_error(Result) of true -> 1; false -> 0 end,
    github_logger:log_build(Service, Result, ExitCode),
    
    case ExitCode of
        1 -> {error, "Build failed: " ++ Service};
        0 -> ok
    end.

%% @doc Start services with GitHub logging
start_services(BasePath, Services) ->
    ServiceStr = string:join(Services, " "),
    io:format("[DOCKER] Starting: ~s~n", [ServiceStr]),
    Cmd = "cd " ++ BasePath ++ " && docker-compose up -d " ++ ServiceStr ++ " 2>&1",
    Result = os:cmd(Cmd),
    log_output(Result),
    
    %% Log to GitHub
    github_logger:log("startup", ServiceStr, Result),
    ok.

%% @doc Check container health with GitHub logging
check_health(ContainerName) ->
    io:format("[DOCKER] Health check: ~s~n", [ContainerName]),
    
    %% Get container status
    StatusCmd = "docker inspect --format='{{.State.Running}}' " ++ ContainerName ++ " 2>/dev/null",
    Status = string:trim(os:cmd(StatusCmd)),
    
    %% Get container logs
    LogsCmd = "docker logs " ++ ContainerName ++ " --tail 100 2>&1",
    Logs = os:cmd(LogsCmd),
    
    %% Log to GitHub
    HealthInfo = lists:flatten(io_lib:format(
        "Container: ~s~n"
        "Running: ~s~n~n"
        "=== CONTAINER LOGS ===~n~s",
        [ContainerName, Status, Logs])),
    github_logger:log("health", ContainerName, HealthInfo),
    
    Status =:= "true".

%% @doc Execute command in container with logging
exec_cmd(Container, Command) ->
    EscapedCmd = escape_shell(Command),
    Cmd = "docker exec " ++ Container ++ " sh -c \"" ++ EscapedCmd ++ "\" 2>&1",
    Result = os:cmd(Cmd),
    
    %% Log to GitHub
    github_logger:log("exec", Container, 
        lists:flatten(io_lib:format("Command: ~s~nResult: ~s", [Command, Result]))),
    
    Result.

%% @doc Check if output indicates build error
is_build_error(Output) when is_list(Output) ->
    Lower = string:lowercase(Output),
    string:find(Lower, "failed to solve") =/= nomatch orelse
    string:find(Lower, "error:") =/= nomatch orelse
    string:find(Lower, "failed") =/= nomatch orelse
    string:find(Lower, "cannot") =/= nomatch;
is_build_error(_) -> true.

%% Internal: log truncated output
log_output(Result) when is_list(Result) ->
    Len = length(Result),
    Out = case Len of
        0 -> "(empty)";
        N when N > 500 -> 
            try
                "..." ++ string:slice(Result, N - 497, 497)
            catch _:_ -> "(truncated)"
            end;
        _ -> Result
    end,
    io:format("[DOCKER] Output (~p chars): ~s~n", [Len, Out]);
log_output(_) ->
    io:format("[DOCKER] Output: (non-string)~n").

%% Internal: escape shell special chars
escape_shell(Str) ->
    lists:flatten([escape_char(C) || C <- Str]).

escape_char($") -> "\\\"";
escape_char($\\) -> "\\\\";
escape_char($$) -> "\\$";
escape_char(C) -> [C].
