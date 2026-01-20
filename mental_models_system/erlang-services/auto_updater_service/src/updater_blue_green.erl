%%%-------------------------------------------------------------------
%%% @doc Blue-Green Deployment Helpers
%%% 
%%% Handles service building, health checks, and traffic switching
%%% for blue-green deployments.
%%% @end
%%%-------------------------------------------------------------------
-module(updater_blue_green).

-export([get_standby_services/1, build_standby/1, start_standby/1]).
-export([check_health/1, switch_traffic/1, read_active_env/0]).

-define(DATA_DIR, "/data").

%%====================================================================
%% Service Management
%%====================================================================

get_standby_services(Env) ->
    [
        "desktop-ui-" ++ Env,
        "api-gateway-" ++ Env,
        "analysis-service-" ++ Env,
        "storage-service-" ++ Env,
        "harvester-service-" ++ Env
    ].

build_standby(Services) ->
    BasePath = "/repo/mental_models_system/erlang-services",
    build_services(BasePath, Services, []).

build_services(_BasePath, [], _Failed) ->
    ok;
build_services(BasePath, [Service | Rest], Failed) ->
    io:format("[UPDATER] ==========================================~n"),
    io:format("[UPDATER] Building: ~s~n", [Service]),
    io:format("[UPDATER] Command: docker-compose build --no-cache ~s~n", [Service]),
    Cmd = "cd " ++ BasePath ++ " && docker-compose build --no-cache " ++ Service ++ " 2>&1",
    Result = os:cmd(Cmd),
    
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
            ErrorFile = ?DATA_DIR ++ "/build_error_" ++ Service ++ ".log",
            file:write_file(ErrorFile, Result),
            io:format("[UPDATER] Error log saved to: ~s~n", [ErrorFile]),
            
            io:format("[UPDATER] Attempting automated fix via LM Studio...~n"),
            FixResult = updater_lm_fix:attempt_lm_studio_fix(Service, Result),
            case FixResult of
                {ok, fixed} ->
                    io:format("[UPDATER] LM Studio fix applied, retrying build...~n"),
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
    true.

start_standby(Services) ->
    BasePath = "/repo/mental_models_system/erlang-services",
    ServiceStr = string:join(Services, " "),
    Cmd = "cd " ++ BasePath ++ " && docker-compose up -d " ++ ServiceStr ++ " 2>&1",
    os:cmd(Cmd).

%%====================================================================
%% Health Checks
%%====================================================================

check_health(Env) ->
    Container = "mental-models-ui-" ++ Env,
    Cmd = "docker inspect --format='{{.State.Running}}' " ++ Container ++ " 2>/dev/null",
    Result = string:trim(os:cmd(Cmd)),
    io:format("[UPDATER] Health check ~s: ~s~n", [Container, Result]),
    Result =:= "true".

%%====================================================================
%% Traffic Switching
%%====================================================================

switch_traffic(NewEnv) ->
    io:format("[UPDATER] Switching traffic to: ~s~n", [NewEnv]),
    
    updater_utils:write_file(?DATA_DIR ++ "/active_env", NewEnv),
    
    Services = [
        {"active.conf", "ui-" ++ NewEnv},
        {"active_api.conf", "api-" ++ NewEnv},
        {"active_analysis.conf", "analysis-" ++ NewEnv},
        {"active_harvester.conf", "harvester-" ++ NewEnv},
        {"active_storage.conf", "storage-" ++ NewEnv},
        {"active_updater.conf", "updater-" ++ NewEnv}
    ],
    
    lists:foreach(fun({ConfigFile, Upstream}) ->
        ConfigContent = "# Active environment: " ++ NewEnv ++ "\nlocation / {\n    proxy_pass http://" ++ Upstream ++ ";\n}\n",
        Cmd = "docker exec mental-models-proxy sh -c 'echo \"" ++ ConfigContent ++ "\" > /etc/nginx/conf.d/" ++ ConfigFile ++ "'",
        Result = os:cmd(Cmd ++ " 2>&1"),
        io:format("[UPDATER] Updated ~s: ~s~n", [ConfigFile, string:sub_string(Result, 1, erlang:min(100, length(Result)))])
    end, Services),
    
    ReloadResult = os:cmd("docker exec mental-models-proxy nginx -s reload 2>&1"),
    io:format("[UPDATER] Nginx reload: ~s~n", [string:sub_string(ReloadResult, 1, erlang:min(200, length(ReloadResult)))]).

read_active_env() ->
    case updater_utils:read_file(?DATA_DIR ++ "/active_env") of
        undefined -> "blue";
        Env -> binary_to_list(Env)
    end.
