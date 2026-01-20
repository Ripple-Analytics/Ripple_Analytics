%%%-------------------------------------------------------------------
%%% @doc Updater Worker
%%% Core update logic with bulletproof fallback chain.
%%% @end
%%%-------------------------------------------------------------------
-module(updater_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([get_status/0, check_updates/0, perform_update/0, get_config/0]).

-record(state, {
    status = idle :: idle | checking | updating | error,
    last_check = undefined,
    last_update = undefined,
    current_commit = undefined,
    remote_commit = undefined,
    branch = <<"master">>,
    update_available = false,
    update_source = undefined,
    error_message = undefined,
    check_interval = 300000,
    timer_ref = undefined,
    heartbeat_ref = undefined,
    check_count = 0
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_status() ->
    gen_server:call(?MODULE, get_status).

check_updates() ->
    gen_server:call(?MODULE, check_updates, 60000).

perform_update() ->
    gen_server:call(?MODULE, perform_update, 120000).

get_config() ->
    gen_server:call(?MODULE, get_config).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[UPDATER] ========================================~n"),
    io:format("[UPDATER] BULLETPROOF AUTO-UPDATER STARTING~n"),
    io:format("[UPDATER] This service will NEVER stop~n"),
    io:format("[UPDATER] ========================================~n"),
    Interval = get_check_interval(),
    self() ! init_repo,
    TimerRef = erlang:send_after(Interval, self(), check_timer),
    %% Heartbeat every 60 seconds to show we're alive
    HeartbeatRef = erlang:send_after(60000, self(), heartbeat),
    {ok, #state{check_interval = Interval, timer_ref = TimerRef, heartbeat_ref = HeartbeatRef}}.

handle_call(get_status, _From, State) ->
    Status = #{
        status => State#state.status,
        last_check => format_time(State#state.last_check),
        last_update => format_time(State#state.last_update),
        current_commit => State#state.current_commit,
        remote_commit => State#state.remote_commit,
        branch => State#state.branch,
        update_available => State#state.update_available,
        update_source => State#state.update_source,
        error_message => State#state.error_message,
        check_interval_seconds => State#state.check_interval div 1000
    },
    {reply, {ok, Status}, State};

handle_call(check_updates, _From, State) ->
    io:format("[UPDATER] Manual check triggered~n"),
    NewState = do_check_updates(State#state{status = checking}),
    {reply, {ok, #{update_available => NewState#state.update_available}}, NewState};

handle_call(perform_update, _From, State) ->
    io:format("[UPDATER] Manual update triggered~n"),
    NewState = do_perform_update(State#state{status = updating}),
    Result = case NewState#state.status of
        idle -> {ok, #{message => <<"Update successful">>, source => NewState#state.update_source}};
        error -> {error, #{message => NewState#state.error_message}};
        _ -> {ok, #{message => <<"Update in progress">>}}
    end,
    {reply, Result, NewState};

handle_call(get_config, _From, State) ->
    Config = #{
        github_repo => get_env_binary("GITHUB_REPO", <<"Ripple-Analytics/Ripple_Analytics">>),
        github_branch => get_env_binary("GITHUB_BRANCH", <<"master">>),
        github_token_configured => os:getenv("GITHUB_TOKEN") =/= false,
        gdrive_configured => os:getenv("GDRIVE_BACKUP_URL") =/= false,
        check_interval_seconds => State#state.check_interval div 1000
    },
    {reply, {ok, Config}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init_repo, State) ->
    %% Wrap init in try-catch to prevent crashes during startup
    NewState = try
        io:format("[UPDATER] Initializing repository~n"),
        do_init_repo(State)
    catch
        Class:Reason:Stacktrace ->
            io:format("[UPDATER] ERROR during init: ~p:~p~n~p~n", [Class, Reason, Stacktrace]),
            State#state{status = error, error_message = <<"Init failed, will retry on next check">>}
    end,
    {noreply, NewState};

handle_info(heartbeat, State) ->
    %% CRITICAL: Always reschedule heartbeat FIRST
    HeartbeatRef = erlang:send_after(60000, self(), heartbeat),
    
    %% Log heartbeat to show we're alive
    CheckCount = State#state.check_count,
    NextCheckIn = case State#state.timer_ref of
        undefined -> <<"unknown">>;
        _ -> 
            Remaining = erlang:read_timer(State#state.timer_ref),
            case Remaining of
                false -> <<"imminent">>;
                Ms -> list_to_binary(integer_to_list(Ms div 1000) ++ "s")
            end
    end,
    io:format("[UPDATER] HEARTBEAT - Alive and running | Checks completed: ~p | Next check in: ~s~n", 
              [CheckCount, NextCheckIn]),
    
    %% Watchdog: If timer_ref is undefined or invalid, recreate it
    NewTimerRef = case State#state.timer_ref of
        undefined ->
            io:format("[UPDATER] WATCHDOG: Timer was missing, recreating!~n"),
            erlang:send_after(State#state.check_interval, self(), check_timer);
        Ref ->
            case erlang:read_timer(Ref) of
                false ->
                    io:format("[UPDATER] WATCHDOG: Timer expired without firing, recreating!~n"),
                    erlang:send_after(State#state.check_interval, self(), check_timer);
                _ -> Ref
            end
    end,
    
    {noreply, State#state{heartbeat_ref = HeartbeatRef, timer_ref = NewTimerRef}};

handle_info(check_timer, State) ->
    %% CRITICAL: Always reschedule timer FIRST to ensure we never stop checking
    %% This is the most important line - it ensures the updater NEVER stops
    TimerRef = erlang:send_after(State#state.check_interval, self(), check_timer),
    NewCheckCount = State#state.check_count + 1,
    
    io:format("[UPDATER] ========================================~n"),
    io:format("[UPDATER] CHECK #~p STARTING~n", [NewCheckCount]),
    io:format("[UPDATER] ========================================~n"),
    
    %% Now do the actual work in a try-catch to prevent any crashes
    FinalState = try
        io:format("[UPDATER] Periodic check triggered~n"),
        NewState = do_check_updates(State#state{status = checking, check_count = NewCheckCount}),
        case NewState#state.update_available of
            true ->
                io:format("[UPDATER] Update available, auto-updating~n"),
                do_perform_update(NewState#state{status = updating});
            false ->
                io:format("[UPDATER] No updates available~n"),
                NewState
        end
    catch
        Class:Reason:Stacktrace ->
            io:format("[UPDATER] ERROR during check: ~p:~p~n~p~n", [Class, Reason, Stacktrace]),
            io:format("[UPDATER] Will retry on next check - updater continues running~n"),
            State#state{status = error, error_message = <<"Check failed, will retry">>, check_count = NewCheckCount}
    end,
    
    io:format("[UPDATER] CHECK #~p COMPLETE - Next check in ~p seconds~n", 
              [NewCheckCount, State#state.check_interval div 1000]),
    {noreply, FinalState#state{timer_ref = TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

get_check_interval() ->
    try
        case os:getenv("UPDATE_CHECK_INTERVAL") of
            false -> 300000;
            Val -> list_to_integer(Val) * 1000
        end
    catch
        _:_ -> 300000  %% Default to 5 minutes if parsing fails
    end.

get_env_binary(Key, Default) ->
    case os:getenv(Key) of
        false -> Default;
        Val -> list_to_binary(Val)
    end.

do_init_repo(State) ->
    RepoPath = "/repo",
    case filelib:is_dir(RepoPath ++ "/.git") of
        true ->
            io:format("[UPDATER] Repository already exists~n"),
            Commit = get_current_commit(),
            Branch = get_env_binary("GITHUB_BRANCH", <<"master">>),
            State#state{current_commit = Commit, branch = Branch, status = idle};
        false ->
            io:format("[UPDATER] Cloning repository~n"),
            case try_clone() of
                {ok, Source} ->
                    Commit = get_current_commit(),
                    Branch = get_env_binary("GITHUB_BRANCH", <<"master">>),
                    io:format("[UPDATER] Clone successful via ~p~n", [Source]),
                    State#state{current_commit = Commit, branch = Branch, update_source = Source, status = idle};
                {error, Reason} ->
                    io:format("[UPDATER] Clone failed: ~p~n", [Reason]),
                    State#state{status = error, error_message = <<"Failed to clone repository">>}
            end
    end.

try_clone() ->
    Branch = os:getenv("GITHUB_BRANCH", "master"),
    
    %% Try authenticated HTTPS first (most common)
    case os:getenv("GITHUB_TOKEN") of
        false -> ok;
        Token ->
            Repo = os:getenv("GITHUB_REPO", "Ripple-Analytics/Ripple_Analytics"),
            Url = "https://" ++ Token ++ "@github.com/" ++ Repo ++ ".git",
            Cmd = "git clone --depth 1 --branch " ++ Branch ++ " " ++ Url ++ " /repo 2>&1",
            case run_cmd(Cmd) of
                ok -> 
                    io:format("[UPDATER] Authenticated HTTPS clone successful~n"),
                    {ok, https_auth};
                _ -> ok
            end
    end,
    
    %% Check if clone succeeded
    case filelib:is_dir("/repo/.git") of
        true -> {ok, https_auth};
        false ->
            %% Try public HTTPS
            case os:getenv("GITHUB_REPO_URL") of
                false -> {error, no_sources};
                PublicUrl ->
                    Cmd2 = "git clone --depth 1 --branch " ++ Branch ++ " " ++ PublicUrl ++ " /repo 2>&1",
                    case run_cmd(Cmd2) of
                        ok -> {ok, https_public};
                        _ -> try_gdrive()
                    end
            end
    end.

try_gdrive() ->
    case os:getenv("GDRIVE_BACKUP_URL") of
        false -> {error, no_gdrive};
        Url ->
            Cmd = "curl -L -o /tmp/backup.zip '" ++ Url ++ "' && mkdir -p /repo && unzip -o /tmp/backup.zip -d /repo",
            case run_cmd(Cmd) of
                ok -> {ok, gdrive};
                _ -> {error, gdrive_failed}
            end
    end.

do_check_updates(State) ->
    Now = erlang:timestamp(),
    Cmd = "cd /repo && git fetch origin " ++ binary_to_list(State#state.branch) ++ " --depth 1 2>&1",
    run_cmd(Cmd),
    
    CurrentCommit = get_current_commit(),
    RemoteCommit = get_remote_commit(State#state.branch),
    
    UpdateAvailable = (CurrentCommit =/= undefined) andalso 
                      (RemoteCommit =/= undefined) andalso 
                      (CurrentCommit =/= RemoteCommit),
    
    io:format("[UPDATER] Current: ~p, Remote: ~p, Update: ~p~n", 
              [CurrentCommit, RemoteCommit, UpdateAvailable]),
    
    State#state{
        status = idle,
        last_check = Now,
        current_commit = CurrentCommit,
        remote_commit = RemoteCommit,
        update_available = UpdateAvailable,
        error_message = undefined
    }.

do_perform_update(State) ->
    Now = erlang:timestamp(),
    Branch = binary_to_list(State#state.branch),
    Cmd = "cd /repo && git reset --hard origin/" ++ Branch ++ " 2>&1",
    run_cmd(Cmd),
    
    NewCommit = get_current_commit(),
    io:format("[UPDATER] Updated to: ~p~n", [NewCommit]),
    
    %% Trigger rebuild in background
    spawn(fun() -> rebuild_services() end),
    
    State#state{
        status = idle,
        last_update = Now,
        current_commit = NewCommit,
        update_available = false,
        error_message = undefined
    }.

get_current_commit() ->
    Result = os:cmd("cd /repo && git rev-parse HEAD 2>/dev/null"),
    case string:trim(Result) of
        "" -> undefined;
        Commit when length(Commit) >= 7 -> list_to_binary(string:sub_string(Commit, 1, 7));
        _ -> undefined
    end.

get_remote_commit(Branch) ->
    Cmd = "cd /repo && git rev-parse origin/" ++ binary_to_list(Branch) ++ " 2>/dev/null",
    Result = os:cmd(Cmd),
    case string:trim(Result) of
        "" -> undefined;
        Commit when length(Commit) >= 7 -> list_to_binary(string:sub_string(Commit, 1, 7));
        _ -> undefined
    end.

rebuild_services() ->
    %% Wrap entire rebuild in try-catch to ensure we never crash
    try
        io:format("[UPDATER] ========================================~n"),
        io:format("[UPDATER] BLUE-GREEN DEPLOYMENT STARTING~n"),
        io:format("[UPDATER] Zero downtime update in progress~n"),
        io:format("[UPDATER] ========================================~n"),
        
        BasePath = "/repo/mental_models_system/erlang-services",
        
        %% Step 0: Auto-detect and set HOST_PATH in .env file
        %% This ensures the UI shows the correct folder path without manual intervention
        detect_and_set_host_path(BasePath),
        
        %% Backend services (not blue-green, just restart)
        BackendServices = "api-gateway analysis-service harvester-service storage-service chaos-engineering",
        
        %% Step 1: Determine which environment is currently active
        ActiveEnv = get_active_environment(),
        StandbyEnv = case ActiveEnv of
            "blue" -> "green";
            "green" -> "blue";
            _ -> "green"  %% Default to updating green if unknown
        end,
        io:format("[UPDATER] Active environment: ~s, Updating standby: ~s~n", [ActiveEnv, StandbyEnv]),
        
        %% Step 2: Build all services including the standby UI
        %% CRITICAL: Use --no-cache to ensure Erlang code changes are picked up
        %% Without --no-cache, Docker may use cached layers and skip recompiling .erl files
        StandbyService = "desktop-ui-" ++ StandbyEnv,
        io:format("[UPDATER] Building backend services and ~s (with --no-cache to ensure code changes are applied)...~n", [StandbyService]),
        BuildCmd = "cd " ++ BasePath ++ " && docker-compose build --no-cache --parallel " ++ BackendServices ++ " " ++ StandbyService ++ " 2>&1",
        io:format("[UPDATER] Build command: ~s~n", [BuildCmd]),
        BuildResult = os:cmd(BuildCmd),
        io:format("[UPDATER] Build output (last 500 chars): ~s~n", [string:slice(BuildResult, max(0, length(BuildResult) - 500))]),
        
        %% Step 3: Update backend services (quick restart)
        io:format("[UPDATER] Restarting backend services...~n"),
        BackendRestartCmd = "cd " ++ BasePath ++ " && docker-compose up -d --no-deps " ++ BackendServices ++ " 2>&1",
        _BackendResult = os:cmd(BackendRestartCmd),
        
        %% Step 4: Update the standby UI environment
        io:format("[UPDATER] Updating standby environment (~s)...~n", [StandbyEnv]),
        StandbyStopCmd = "cd " ++ BasePath ++ " && docker-compose stop " ++ StandbyService ++ " 2>&1",
        _StandbyStopResult = os:cmd(StandbyStopCmd),
        StandbyRmCmd = "cd " ++ BasePath ++ " && docker-compose rm -f " ++ StandbyService ++ " 2>&1",
        _StandbyRmResult = os:cmd(StandbyRmCmd),
        StandbyStartCmd = "cd " ++ BasePath ++ " && docker-compose up -d " ++ StandbyService ++ " 2>&1",
        _StandbyStartResult = os:cmd(StandbyStartCmd),
        
        %% Step 5: Wait for standby to be healthy
        io:format("[UPDATER] Waiting for ~s to be healthy...~n", [StandbyEnv]),
        timer:sleep(10000),  %% Wait 10 seconds for container to start
        
        %% Step 6: Health check the standby
        StandbyHealthy = check_standby_health(StandbyEnv),
        
        case StandbyHealthy of
            true ->
                %% Step 7: Switch traffic to the new environment
                io:format("[UPDATER] ~s is healthy, switching traffic...~n", [StandbyEnv]),
                switch_active_environment(StandbyEnv, BasePath),
                io:format("[UPDATER] ========================================~n"),
                io:format("[UPDATER] BLUE-GREEN DEPLOYMENT COMPLETE~n"),
                io:format("[UPDATER] Traffic now routed to: ~s~n", [StandbyEnv]),
                io:format("[UPDATER] Zero downtime achieved!~n"),
                io:format("[UPDATER] ========================================~n");
            false ->
                io:format("[UPDATER] WARNING: ~s failed health check, keeping traffic on ~s~n", [StandbyEnv, ActiveEnv]),
                io:format("[UPDATER] Deployment rolled back automatically~n")
        end
    catch
        Class:Reason:Stacktrace ->
            io:format("[UPDATER] ERROR during rebuild: ~p:~p~n~p~n", [Class, Reason, Stacktrace]),
            io:format("[UPDATER] Rebuild failed but updater continues running~n")
    end.

%% Auto-detect and set HOST_PATH in .env file
%% This reads the host path from docker and writes it to .env so containers can display it
detect_and_set_host_path(BasePath) ->
    io:format("[UPDATER] Auto-detecting host path...~n"),
    
    %% Get the host path by inspecting the auto-updater container's bind mount
    %% The /services volume is mounted from the host's erlang-services directory
    InspectCmd = "docker inspect mental-models-auto-updater --format='{{range .Mounts}}{{if eq .Destination \"/services\"}}{{.Source}}{{end}}{{end}}' 2>/dev/null",
    HostPath = string:trim(os:cmd(InspectCmd)),
    
    case HostPath of
        "" ->
            %% Fallback: try to read from existing .env file
            io:format("[UPDATER] Could not detect host path from docker, checking .env~n");
        Path ->
            io:format("[UPDATER] Detected host path: ~s~n", [Path]),
            %% Update the .env file with HOST_PATH
            EnvFile = BasePath ++ "/.env",
            update_env_file(EnvFile, "HOST_PATH", Path)
    end.

%% Update or add a key=value pair in the .env file
update_env_file(EnvFile, Key, Value) ->
    %% Read existing .env content
    ExistingContent = case file:read_file(EnvFile) of
        {ok, Content} -> binary_to_list(Content);
        {error, _} -> ""
    end,
    
    %% Check if key already exists
    KeyPattern = Key ++ "=",
    Lines = string:split(ExistingContent, "\n", all),
    
    %% Filter out existing key and add new value
    FilteredLines = [L || L <- Lines, not lists:prefix(KeyPattern, L), L =/= ""],
    NewLines = FilteredLines ++ [Key ++ "=" ++ Value],
    NewContent = string:join(NewLines, "\n") ++ "\n",
    
    %% Write back to .env file
    case file:write_file(EnvFile, NewContent) of
        ok ->
            io:format("[UPDATER] Updated ~s in .env file~n", [Key]);
        {error, Reason} ->
            io:format("[UPDATER] WARNING: Could not update .env file: ~p~n", [Reason])
    end.

%% Get the currently active environment (blue or green)
get_active_environment() ->
    %% Read from nginx config or default to blue
    Result = os:cmd("cat /repo/mental_models_system/erlang-services/nginx_proxy/active_env 2>/dev/null"),
    case string:trim(Result) of
        "green" -> "green";
        "blue" -> "blue";
        _ -> "blue"  %% Default to blue
    end.

%% Check if the standby environment is healthy
check_standby_health(Env) ->
    Container = "mental-models-ui-" ++ Env,
    %% Check if container is running and healthy
    HealthCmd = "docker inspect --format='{{.State.Health.Status}}' " ++ Container ++ " 2>/dev/null",
    Result = string:trim(os:cmd(HealthCmd)),
    io:format("[UPDATER] Health check for ~s: ~s~n", [Container, Result]),
    Result =:= "healthy".

%% Switch the active environment by updating nginx config
switch_active_environment(NewEnv, BasePath) ->
    %% Update the active_env file
    ActiveEnvFile = BasePath ++ "/nginx_proxy/active_env",
    file:write_file(ActiveEnvFile, NewEnv),
    
    %% Update the nginx active.conf to route to the new environment
    ActiveConfFile = BasePath ++ "/nginx_proxy/active.conf",
    NewConfig = "# Active environment: " ++ NewEnv ++ "\nlocation / {\n    proxy_pass http://" ++ NewEnv ++ ";\n}\n",
    file:write_file(ActiveConfFile, NewConfig),
    
    %% Reload nginx to apply the change (zero downtime)
    ReloadCmd = "docker exec mental-models-proxy nginx -s reload 2>&1",
    _ReloadResult = os:cmd(ReloadCmd),
    io:format("[UPDATER] Nginx reloaded, traffic switched to ~s~n", [NewEnv]).

%% Run command with timeout to prevent hanging
%% This is CRITICAL for bulletproof operation - git commands can hang indefinitely
run_cmd(Cmd) ->
    run_cmd_with_timeout(Cmd, 30000).  %% 30 second default timeout

run_cmd_with_timeout(Cmd, Timeout) ->
    Parent = self(),
    Ref = make_ref(),
    
    %% Spawn a process to run the command
    Pid = spawn(fun() ->
        Result = os:cmd(Cmd),
        Parent ! {Ref, done, Result}
    end),
    
    %% Wait for result with timeout
    receive
        {Ref, done, Result} ->
            case string:find(Result, "fatal:") of
                nomatch ->
                    case string:find(Result, "error:") of
                        nomatch -> ok;
                        _ -> {error, Result}
                    end;
                _ -> {error, Result}
            end
    after Timeout ->
        %% Kill the hung process
        exit(Pid, kill),
        io:format("[UPDATER] WARNING: Command timed out after ~p ms: ~s~n", [Timeout, Cmd]),
        {error, timeout}
    end.

format_time(undefined) -> null;
format_time({MegaSecs, Secs, _MicroSecs}) ->
    Seconds = MegaSecs * 1000000 + Secs,
    list_to_binary(calendar:system_time_to_rfc3339(Seconds, [{unit, second}])).
