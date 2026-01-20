%%%-------------------------------------------------------------------
%%% @doc GitHub Logger - Pushes verbose debug logs to GitHub repo
%%% 
%%% Logs are written to mirrored folder structure:
%%%   debug_logs/blue/       - Blue environment state
%%%   debug_logs/blue_logs/  - Blue environment logs
%%%   debug_logs/green/      - Green environment state
%%%   debug_logs/green_logs/ - Green environment logs
%%% @end
%%%-------------------------------------------------------------------
-module(github_logger).

-export([log/4, log_error/4, log_build/4, log_deploy/4]).
-export([log/3, log_error/3, log_build/3, log_deploy/3]).  %% Legacy without env
-export([push_logs/0, set_env/1, get_env/0]).

-define(REPO_PATH, "/repo").
-define(LOG_BASE, "/repo/mental_models_system/erlang-services/debug_logs").

%% Process dictionary key for current environment
-define(ENV_KEY, current_deploy_env).

%% @doc Set current deployment environment (blue or green)
set_env(Env) when Env =:= "blue"; Env =:= "green" ->
    put(?ENV_KEY, Env),
    ok;
set_env(Env) when is_list(Env) ->
    put(?ENV_KEY, Env),
    ok.

%% @doc Get current deployment environment
get_env() ->
    case get(?ENV_KEY) of
        undefined -> "blue";
        Env -> Env
    end.

%% @doc Log with explicit environment
log(Env, Service, Category, Message) ->
    write_log(Env, Service, Category, Message, "INFO").

%% @doc Log error with explicit environment
log_error(Env, Service, Category, Message) ->
    write_log(Env, Service, Category, Message, "ERROR"),
    push_logs().

%% @doc Log build output with explicit environment
log_build(Env, Service, Output, ExitCode) ->
    Msg = io_lib:format(
        "BUILD OUTPUT~n"
        "============~n"
        "Exit Code: ~p~n~n"
        "Output:~n~s~n",
        [ExitCode, Output]),
    Level = case ExitCode of 0 -> "INFO"; _ -> "ERROR" end,
    write_log(Env, Service, "build", lists:flatten(Msg), Level),
    push_logs().

%% @doc Log deployment status with explicit environment
log_deploy(Env, Service, Status, Details) ->
    Msg = io_lib:format(
        "DEPLOYMENT STATUS~n"
        "=================~n"
        "Environment: ~s~n"
        "Service: ~s~n"
        "Status: ~s~n~n"
        "Details:~n~s~n",
        [Env, Service, Status, Details]),
    Level = case Status of "success" -> "INFO"; _ -> "ERROR" end,
    write_log(Env, Service, "deploy", lists:flatten(Msg), Level),
    push_logs().

%% Legacy functions - use current env from process dictionary
log(Service, Category, Message) ->
    log(get_env(), Service, Category, Message).

log_error(Service, Category, Message) ->
    log_error(get_env(), Service, Category, Message).

log_build(Service, Output, ExitCode) ->
    log_build(get_env(), Service, Output, ExitCode).

log_deploy(Service, Status, Details) ->
    log_deploy(get_env(), Service, Status, Details).

%% @doc Push all logs to GitHub
push_logs() ->
    spawn(fun() -> do_push_logs() end).

%%====================================================================
%% Internal Functions
%%====================================================================

write_log(Env, Service, Category, Message, Level) ->
    %% Determine paths based on environment
    %% State goes to blue/ or green/, logs go to blue_logs/ or green_logs/
    LogsDir = ?LOG_BASE ++ "/" ++ Env ++ "_logs/" ++ normalize_service(Service),
    StateDir = ?LOG_BASE ++ "/" ++ Env ++ "/" ++ normalize_service(Service),
    
    %% Ensure directories exist
    filelib:ensure_dir(LogsDir ++ "/"),
    filelib:ensure_dir(StateDir ++ "/"),
    
    %% Build log content
    Timestamp = format_timestamp(),
    LogFile = LogsDir ++ "/" ++ Category ++ "_" ++ Timestamp ++ ".log",
    LatestFile = LogsDir ++ "/" ++ Category ++ "_latest.log",
    
    Content = io_lib:format(
        "================================================================================~n"
        "~s LOG | ENV: ~s | SERVICE: ~s | CATEGORY: ~s~n"
        "Timestamp: ~s~n"
        "================================================================================~n~n"
        "~s~n~n"
        "================================================================================~n"
        "SYSTEM INFO~n"
        "================================================================================~n"
        "~s~n"
        "================================================================================~n"
        "DOCKER STATUS~n"
        "================================================================================~n"
        "~s~n"
        "================================================================================~n"
        "END OF LOG~n"
        "================================================================================~n",
        [Level, Env, Service, Category, Timestamp, Message, 
         get_system_info(), get_docker_status()]),
    
    FlatContent = lists:flatten(Content),
    
    %% Write timestamped log
    file:write_file(LogFile, FlatContent),
    
    %% Write latest log (overwrites)
    file:write_file(LatestFile, FlatContent),
    
    %% Also write state summary to state dir
    StateSummary = io_lib:format(
        "~s | ~s | ~s~n~s~n",
        [Timestamp, Level, Category, truncate(Message, 500)]),
    StateFile = StateDir ++ "/state.log",
    file:write_file(StateFile, lists:flatten(StateSummary), [append]),
    
    io:format("[GITHUB_LOGGER] Wrote: ~s/~s/~s~n", [Env, Service, Category]).

normalize_service(Service) ->
    %% Convert service name to folder-safe format
    S1 = string:replace(Service, "-", "_", all),
    S2 = string:replace(S1, " ", "_", all),
    lists:flatten(S2).

do_push_logs() ->
    %% Small delay to batch multiple logs
    timer:sleep(2000),
    
    %% Configure git
    os:cmd("cd " ++ ?REPO_PATH ++ " && git config user.email 'debug@mental-models.local'"),
    os:cmd("cd " ++ ?REPO_PATH ++ " && git config user.name 'Debug Logger'"),
    
    %% Add all logs
    os:cmd("cd " ++ ?REPO_PATH ++ " && git add mental_models_system/erlang-services/debug_logs/ 2>/dev/null"),
    
    %% Commit
    CommitMsg = "debug: Auto-push logs " ++ format_timestamp(),
    CommitCmd = "cd " ++ ?REPO_PATH ++ " && git commit -m '" ++ CommitMsg ++ "' 2>&1",
    CommitResult = os:cmd(CommitCmd),
    
    case string:find(CommitResult, "nothing to commit") of
        nomatch ->
            %% Push to GitHub
            PushCmd = "cd " ++ ?REPO_PATH ++ " && git push origin release2 2>&1",
            PushResult = os:cmd(PushCmd),
            io:format("[GITHUB_LOGGER] Push: ~s~n", [truncate(PushResult, 200)]);
        _ ->
            io:format("[GITHUB_LOGGER] No new logs to push~n")
    end.

get_system_info() ->
    lists:flatten(io_lib:format(
        "Hostname: ~s~n"
        "Erlang: ~s~n"
        "Time: ~s~n",
        [
            string:trim(os:cmd("hostname 2>/dev/null || echo unknown")),
            erlang:system_info(otp_release),
            format_timestamp()
        ])).

get_docker_status() ->
    os:cmd("docker ps -a --format 'table {{.Names}}\t{{.Status}}' 2>&1 || echo 'Docker not available'").

format_timestamp() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:local_time(),
    lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B_~2..0B-~2..0B-~2..0B",
                                [Y, M, D, H, Mi, S])).

truncate(Str, Max) when is_list(Str), length(Str) > Max ->
    string:slice(Str, 0, Max) ++ "...";
truncate(Str, _) when is_list(Str) -> Str;
truncate(_, _) -> "".
