%%%-------------------------------------------------------------------
%%% @doc Docker Utilities - Build, start, health check operations
%%% @end
%%%-------------------------------------------------------------------
-module(docker_utils).

-export([build_service/2, start_services/2, check_health/1]).
-export([exec_cmd/2, is_build_error/1]).

%% @doc Build a single service
build_service(BasePath, Service) ->
    io:format("[DOCKER] Building: ~s~n", [Service]),
    Cmd = "cd " ++ BasePath ++ " && docker-compose build --no-cache " ++ Service ++ " 2>&1",
    Result = os:cmd(Cmd),
    log_output(Result),
    case is_build_error(Result) of
        true -> {error, "Build failed: " ++ Service};
        false -> ok
    end.

%% @doc Start services
start_services(BasePath, Services) ->
    ServiceStr = string:join(Services, " "),
    Cmd = "cd " ++ BasePath ++ " && docker-compose up -d " ++ ServiceStr ++ " 2>&1",
    Result = os:cmd(Cmd),
    log_output(Result),
    ok.

%% @doc Check container health
check_health(ContainerName) ->
    Cmd = "docker inspect --format='{{.State.Running}}' " ++ ContainerName ++ " 2>/dev/null",
    Result = string:trim(os:cmd(Cmd)),
    Result =:= "true".

%% @doc Execute command in container
exec_cmd(Container, Command) ->
    EscapedCmd = escape_shell(Command),
    os:cmd("docker exec " ++ Container ++ " sh -c \"" ++ EscapedCmd ++ "\" 2>&1").

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
