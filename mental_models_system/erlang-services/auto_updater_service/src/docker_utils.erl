%%%-------------------------------------------------------------------
%%% @doc Docker Utilities - Build, start, health check operations
%%% @end
%%%-------------------------------------------------------------------
-module(docker_utils).

-export([build_service/2, start_services/2, check_health/1]).
-export([exec_cmd/2, is_build_error/1]).

-define(BASE_PATH, "/repo/mental_models_system/erlang-services").

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
    os:cmd(Cmd).

%% @doc Check container health
check_health(ContainerName) ->
    Cmd = "docker inspect --format='{{.State.Running}}' " ++ ContainerName ++ " 2>/dev/null",
    string:trim(os:cmd(Cmd)) =:= "true".

%% @doc Execute command in container
exec_cmd(Container, Command) ->
    os:cmd("docker exec " ++ Container ++ " sh -c '" ++ Command ++ "' 2>&1").

%% @doc Check if output indicates build error
is_build_error(Output) when is_list(Output) ->
    string:find(Output, "failed to solve") =/= nomatch orelse
    string:find(Output, "error:") =/= nomatch orelse
    string:find(Output, "ERROR:") =/= nomatch orelse
    string:find(Output, "FAILED") =/= nomatch;
is_build_error(_) -> true.

%% Internal: log truncated output
log_output(Result) ->
    Safe = case is_list(Result) of true -> Result; false -> "" end,
    Len = length(Safe),
    Out = case Len of
        0 -> "(empty)";
        N when N > 500 -> string:sub_string(Safe, N - 499, N);
        _ -> Safe
    end,
    io:format("[DOCKER] Output: ~s~n", [Out]).
