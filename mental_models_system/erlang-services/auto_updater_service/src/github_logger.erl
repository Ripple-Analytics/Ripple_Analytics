%%%-------------------------------------------------------------------
%%% @doc GitHub Logger - Pushes verbose debug logs to GitHub repo
%%% 
%%% Uses fresh clone approach - writes logs locally, then clones fresh
%%% repo, copies logs, and pushes. Same approach as debug_logger.
%%% @end
%%%-------------------------------------------------------------------
-module(github_logger).

-export([log/4, log_error/4, log_build/4, log_deploy/4]).
-export([log/3, log_error/3, log_build/3, log_deploy/3]).
-export([push_logs/0, set_env/1, get_env/0]).

-define(LOCAL_LOGS, "/tmp/updater_logs").
-define(CLONE_PATH, "/tmp/updater_repo").
-define(REPO_LOGS, "/tmp/updater_repo/mental_models_system/erlang-services/debug_logs").
-define(ENV_KEY, current_deploy_env).

%% Set current deployment environment
set_env(Env) ->
    put(?ENV_KEY, Env),
    ok.

%% Get current deployment environment
get_env() ->
    case get(?ENV_KEY) of
        undefined -> "blue";
        Env -> Env
    end.

%% Log with explicit environment
log(Env, Service, Category, Message) ->
    write_log(Env, Service, Category, Message, "INFO").

%% Log error with explicit environment
log_error(Env, Service, Category, Message) ->
    write_log(Env, Service, Category, Message, "ERROR"),
    push_logs().

%% Log build output with explicit environment
log_build(Env, Service, Output, ExitCode) ->
    Msg = io_lib:format("BUILD | Exit: ~p~n~s", [ExitCode, Output]),
    Level = case ExitCode of 0 -> "INFO"; _ -> "ERROR" end,
    write_log(Env, Service, "build", lists:flatten(Msg), Level),
    push_logs().

%% Log deployment status with explicit environment
log_deploy(Env, Service, Status, Details) ->
    Msg = io_lib:format("DEPLOY | ~s | ~s~n~s", [Env, Status, Details]),
    Level = case Status of "success" -> "INFO"; _ -> "ERROR" end,
    write_log(Env, Service, "deploy", lists:flatten(Msg), Level),
    push_logs().

%% Legacy functions
log(Service, Category, Message) ->
    log(get_env(), Service, Category, Message).

log_error(Service, Category, Message) ->
    log_error(get_env(), Service, Category, Message).

log_build(Service, Output, ExitCode) ->
    log_build(get_env(), Service, Output, ExitCode).

log_deploy(Service, Status, Details) ->
    log_deploy(get_env(), Service, Status, Details).

%% Push all logs to GitHub
push_logs() ->
    spawn(fun() -> do_push_logs() end).

%%====================================================================
%% Internal Functions
%%====================================================================

write_log(Env, Service, Category, Message, Level) ->
    try
        %% Write to LOCAL directory (not shared volume)
        LogsDir = ?LOCAL_LOGS ++ "/" ++ Env ++ "_logs/" ++ safe_name(Service),
        os:cmd("mkdir -p " ++ LogsDir),
        
        Timestamp = format_timestamp(),
        LogFile = LogsDir ++ "/" ++ Category ++ "_" ++ Timestamp ++ ".log",
        
        Content = io_lib:format(
            "~s | ~s | ~s | ~s~n"
            "Timestamp: ~s~n"
            "Hostname: ~s~n"
            "Erlang: ~s~n~n"
            "~s~n",
            [Level, Env, Service, Category, Timestamp,
             string:trim(os:cmd("hostname 2>/dev/null || echo unknown")),
             erlang:system_info(otp_release),
             Message]),
        
        file:write_file(LogFile, lists:flatten(Content)),
        io:format("[GITHUB_LOGGER] Logged: ~s/~s/~s~n", [Env, Service, Category])
    catch _:_ -> ok end.

do_push_logs() ->
    try
        timer:sleep(2000),
        
        RawToken = case os:getenv("GITHUB_TOKEN") of
            false -> "";
            T -> T
        end,
        Token = string:trim(RawToken, both, " \t\n\r"),
        io:format("[GITHUB_LOGGER] Token length: ~p~n", [length(Token)]),
        
        case length(Token) > 30 of
            true ->
                %% Remove old clone
                os:cmd("rm -rf " ++ ?CLONE_PATH),
                
                %% Clone fresh
                CloneUrl = "https://" ++ Token ++ "@github.com/Ripple-Analytics/Ripple_Analytics.git",
                CloneCmd = "git clone --depth 1 --branch release2 " ++ CloneUrl ++ " " ++ ?CLONE_PATH ++ " 2>&1",
                CloneResult = os:cmd(CloneCmd),
                io:format("[GITHUB_LOGGER] Clone: ~s~n", [truncate(CloneResult, 80)]),
                
                %% Configure git
                os:cmd("cd " ++ ?CLONE_PATH ++ " && git config user.email 'updater@mental-models.local'"),
                os:cmd("cd " ++ ?CLONE_PATH ++ " && git config user.name 'Auto Updater'"),
                
                %% Ensure repo logs directory exists
                os:cmd("mkdir -p " ++ ?REPO_LOGS),
                
                %% Copy local logs to cloned repo
                CopyResult = os:cmd("cp -r " ++ ?LOCAL_LOGS ++ "/* " ++ ?REPO_LOGS ++ "/ 2>&1"),
                io:format("[GITHUB_LOGGER] Copy: ~s~n", [truncate(CopyResult, 80)]),
                
                %% Add, commit, push
                os:cmd("cd " ++ ?CLONE_PATH ++ " && git add -A"),
                CommitMsg = "updater: " ++ format_timestamp(),
                CommitResult = os:cmd("cd " ++ ?CLONE_PATH ++ " && git commit -m '" ++ CommitMsg ++ "' 2>&1"),
                io:format("[GITHUB_LOGGER] Commit: ~s~n", [truncate(CommitResult, 80)]),
                
                PushResult = os:cmd("cd " ++ ?CLONE_PATH ++ " && git push origin release2 2>&1"),
                io:format("[GITHUB_LOGGER] Push: ~s~n", [truncate(PushResult, 80)]),
                ok;
            false ->
                io:format("[GITHUB_LOGGER] No valid token~n"),
                ok
        end
    catch _:_ -> {error, push_failed} end.

safe_name(Name) ->
    try
        S1 = string:replace(Name, "-", "_", all),
        S2 = string:replace(S1, " ", "_", all),
        lists:flatten(S2)
    catch _:_ -> "unknown" end.

format_timestamp() ->
    try
        {{Y, M, D}, {H, Mi, S}} = calendar:local_time(),
        lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B_~2..0B-~2..0B-~2..0B",
                                    [Y, M, D, H, Mi, S]))
    catch _:_ -> "unknown_time" end.

truncate(Str, Max) when is_list(Str) ->
    case length(Str) > Max of
        true -> string:slice(Str, 0, Max) ++ "...";
        false -> Str
    end;
truncate(_, _) -> "...".
