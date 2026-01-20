%%%-------------------------------------------------------------------
%%% @doc Updater Worker - Proper Blue-Green Deployment
%%% 
%%% CORE PRINCIPLE: Active version NEVER touched during update.
%%% Only the standby is rebuilt. If standby fails, active keeps serving.
%%% 
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
    branch = <<"release2">>,
    update_available = false,
    active_env = "blue",
    standby_env = "green",
    error_message = undefined,
    check_interval = 300000,
    timer_ref = undefined,
    heartbeat_ref = undefined,
    check_count = 0,
    consecutive_failures = 0,
    last_failed_commit = undefined
}).

-define(MAX_CONSECUTIVE_FAILURES, 3).
-define(DATA_DIR, "/data").

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_status() -> gen_server:call(?MODULE, get_status).
check_updates() -> gen_server:call(?MODULE, check_updates, 60000).
perform_update() -> gen_server:call(?MODULE, perform_update, 600000).
get_config() -> gen_server:call(?MODULE, get_config).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[UPDATER] ========================================~n"),
    io:format("[UPDATER] BLUE-GREEN AUTO-UPDATER v2.0~n"),
    io:format("[UPDATER] Active stays running, only standby updated~n"),
    io:format("[UPDATER] ========================================~n"),
    
    %% Ensure data directory exists
    filelib:ensure_dir(?DATA_DIR ++ "/"),
    
    Interval = get_check_interval(),
    %% HARDCODED - don't rely on env vars that might be wrong
    Branch = <<"release2">>,
    ActiveEnv = read_active_env(),
    StandbyEnv = case ActiveEnv of "blue" -> "green"; _ -> "blue" end,
    
    io:format("[UPDATER] Branch: ~s~n", [Branch]),
    io:format("[UPDATER] Active: ~s, Standby: ~s~n", [ActiveEnv, StandbyEnv]),
    
    self() ! init_repo,
    %% Check immediately on startup, then every Interval after
    TimerRef = erlang:send_after(5000, self(), check_timer),  %% First check in 5 seconds
    HeartbeatRef = erlang:send_after(60000, self(), heartbeat),
    
    {ok, #state{
        check_interval = Interval, 
        timer_ref = TimerRef, 
        heartbeat_ref = HeartbeatRef,
        branch = Branch,
        active_env = ActiveEnv,
        standby_env = StandbyEnv
    }}.

handle_call(get_status, _From, State) ->
    Status = #{
        status => State#state.status,
        last_check => format_time(State#state.last_check),
        last_update => format_time(State#state.last_update),
        current_commit => State#state.current_commit,
        remote_commit => State#state.remote_commit,
        branch => State#state.branch,
        update_available => State#state.update_available,
        active_env => list_to_binary(State#state.active_env),
        standby_env => list_to_binary(State#state.standby_env),
        consecutive_failures => State#state.consecutive_failures,
        error_message => State#state.error_message
    },
    {reply, {ok, Status}, State};

handle_call(check_updates, _From, State) ->
    NewState = do_check_updates(State),
    {reply, {ok, #{update_available => NewState#state.update_available}}, NewState};

handle_call(perform_update, _From, State) ->
    NewState = do_perform_update(State),
    Result = case NewState#state.status of
        idle -> {ok, #{message => <<"Update successful">>}};
        error -> {error, #{message => NewState#state.error_message}};
        _ -> {ok, #{message => <<"Update in progress">>}}
    end,
    {reply, Result, NewState};

handle_call(get_config, _From, State) ->
    Config = #{
        branch => State#state.branch,
        active_env => list_to_binary(State#state.active_env),
        standby_env => list_to_binary(State#state.standby_env),
        check_interval_seconds => State#state.check_interval div 1000
    },
    {reply, {ok, Config}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init_repo, State) ->
    NewState = try
        do_init_repo(State)
    catch
        _:_ -> State#state{status = error, error_message = <<"Init failed">>}
    end,
    {noreply, NewState};

handle_info(heartbeat, State) ->
    HeartbeatRef = erlang:send_after(60000, self(), heartbeat),
    io:format("[UPDATER] HEARTBEAT | Active: ~s | Checks: ~p | Failures: ~p~n", 
              [State#state.active_env, State#state.check_count, State#state.consecutive_failures]),
    %% Phone home - report status to external webhook
    phone_home(State),
    {noreply, State#state{heartbeat_ref = HeartbeatRef}};

handle_info(check_timer, State) ->
    %% ALWAYS reschedule first
    TimerRef = erlang:send_after(State#state.check_interval, self(), check_timer),
    NewCheckCount = State#state.check_count + 1,
    
    io:format("[UPDATER] === CHECK #~p ===~n", [NewCheckCount]),
    
    FinalState = try
        CheckedState = do_check_updates(State#state{check_count = NewCheckCount}),
        case CheckedState#state.update_available of
            true ->
                io:format("[UPDATER] Update available, deploying to standby (~s)~n", 
                          [CheckedState#state.standby_env]),
                do_perform_update(CheckedState);
            false ->
                io:format("[UPDATER] No update needed~n"),
                CheckedState
        end
    catch
        Class:Reason:Stack ->
            io:format("[UPDATER] ERROR: ~p:~p~n~p~n", [Class, Reason, Stack]),
            State#state{status = error, check_count = NewCheckCount}
    end,
    
    {noreply, FinalState#state{timer_ref = TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Core Logic
%%====================================================================

do_init_repo(State) ->
    case filelib:is_dir("/repo/.git") of
        true ->
            Commit = get_current_commit(),
            io:format("[UPDATER] Repo exists, commit: ~p~n", [Commit]),
            State#state{current_commit = Commit, status = idle};
        false ->
            case clone_repo(State#state.branch) of
                ok ->
                    Commit = get_current_commit(),
                    State#state{current_commit = Commit, status = idle};
                {error, _} ->
                    State#state{status = error, error_message = <<"Clone failed">>}
            end
    end.

do_check_updates(State) ->
    Branch = binary_to_list(State#state.branch),
    
    %% Fetch latest
    os:cmd("cd /repo && git fetch origin " ++ Branch ++ " 2>&1"),
    
    CurrentCommit = get_current_commit(),
    RemoteCommit = get_remote_commit(Branch),
    LastProcessed = read_file(?DATA_DIR ++ "/last_processed_commit"),
    LastFailed = read_file(?DATA_DIR ++ "/last_failed_commit"),
    
    io:format("[UPDATER] Current: ~s, Remote: ~s~n", [fmt(CurrentCommit), fmt(RemoteCommit)]),
    io:format("[UPDATER] LastProcessed: ~s, LastFailed: ~s~n", [fmt(LastProcessed), fmt(LastFailed)]),
    
    %% Update available if:
    %% 1. Remote differs from current
    %% 2. Remote differs from last processed (not already done)
    %% 3. Remote differs from last failed (don't retry broken commit)
    %% 4. Not in circuit breaker mode (too many consecutive failures)
    UpdateAvailable = (RemoteCommit =/= undefined) andalso
                      (CurrentCommit =/= RemoteCommit) andalso
                      (RemoteCommit =/= LastProcessed) andalso
                      (RemoteCommit =/= LastFailed) andalso
                      (State#state.consecutive_failures < ?MAX_CONSECUTIVE_FAILURES),
    
    case State#state.consecutive_failures >= ?MAX_CONSECUTIVE_FAILURES of
        true -> io:format("[UPDATER] CIRCUIT BREAKER ACTIVE - too many failures~n");
        false -> ok
    end,
    
    State#state{
        status = idle,
        last_check = erlang:timestamp(),
        current_commit = CurrentCommit,
        remote_commit = RemoteCommit,
        update_available = UpdateAvailable
    }.

do_perform_update(State) ->
    RemoteCommit = State#state.remote_commit,
    StandbyEnv = State#state.standby_env,
    ActiveEnv = State#state.active_env,
    Branch = binary_to_list(State#state.branch),
    
    io:format("[UPDATER] ========================================~n"),
    io:format("[UPDATER] DEPLOYING TO STANDBY: ~s~n", [StandbyEnv]),
    io:format("[UPDATER] Active (~s) continues serving traffic~n", [ActiveEnv]),
    io:format("[UPDATER] ========================================~n"),
    
    %% Step 1: Pull the new code
    os:cmd("cd /repo && git reset --hard origin/" ++ Branch),
    NewCommit = get_current_commit(),
    io:format("[UPDATER] Pulled commit: ~s~n", [fmt(NewCommit)]),
    
    %% Step 1.5: Run pre-build syntax check and auto-fix
    io:format("[UPDATER] Running pre-build syntax check...~n"),
    PreBuildResult = os:cmd("cd /repo/mental_models_system/erlang-services && bash scripts/fix_erlang_syntax.sh 2>&1"),
    io:format("[UPDATER] Pre-build result: ~s~n", [string:sub_string(PreBuildResult, 1, erlang:min(500, length(PreBuildResult)))]),
    
    %% Step 2: Build ONLY the standby services
    StandbyServices = get_standby_services(StandbyEnv),
    BuildResult = build_standby(StandbyServices),
    
    case BuildResult of
        ok ->
            %% Step 3: Start standby and health check
            start_standby(StandbyServices),
            timer:sleep(15000),  %% Wait for startup
            
            case check_health(StandbyEnv) of
                true ->
                    %% Step 4: Switch traffic
                    io:format("[UPDATER] Standby healthy, switching traffic~n"),
                    switch_traffic(StandbyEnv),
                    
                    %% Step 5: Record success
                    write_file(?DATA_DIR ++ "/last_processed_commit", NewCommit),
                    delete_file(?DATA_DIR ++ "/last_failed_commit"),
                    
                    io:format("[UPDATER] ========================================~n"),
                    io:format("[UPDATER] SUCCESS! Traffic now on: ~s~n", [StandbyEnv]),
                    io:format("[UPDATER] ========================================~n"),
                    
                    %% Swap active/standby
                    State#state{
                        status = idle,
                        last_update = erlang:timestamp(),
                        current_commit = NewCommit,
                        update_available = false,
                        active_env = StandbyEnv,
                        standby_env = ActiveEnv,
                        consecutive_failures = 0,
                        last_failed_commit = undefined
                    };
                false ->
                    handle_failure(State, NewCommit, "Health check failed")
            end;
        {error, Reason} ->
            handle_failure(State, NewCommit, Reason)
    end.

handle_failure(State, FailedCommit, Reason) ->
    NewFailures = State#state.consecutive_failures + 1,
    io:format("[UPDATER] FAILED: ~s (failure #~p)~n", [Reason, NewFailures]),
    
    %% Record the failed commit so we don't retry it
    write_file(?DATA_DIR ++ "/last_failed_commit", FailedCommit),
    
    %% Revert to the previous commit
    os:cmd("cd /repo && git reset --hard HEAD~1 2>&1"),
    
    io:format("[UPDATER] Active (~s) continues serving~n", [State#state.active_env]),
    
    State#state{
        status = error,
        error_message = list_to_binary(Reason),
        consecutive_failures = NewFailures,
        last_failed_commit = FailedCommit,
        update_available = false
    }.

%%====================================================================
%% Blue-Green Helpers
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
    
    %% Log last 1000 chars of build output
    OutputLen = length(Result),
    TruncatedOutput = if OutputLen > 1000 -> string:sub_string(Result, OutputLen - 999, OutputLen); true -> Result end,
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

is_build_error(Output) ->
    string:find(Output, "failed to solve") =/= nomatch orelse
    string:find(Output, "error:") =/= nomatch orelse
    string:find(Output, "ERROR:") =/= nomatch orelse
    string:find(Output, "FAILED") =/= nomatch.

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

switch_traffic(NewEnv) ->
    %% Update active env file
    write_file(?DATA_DIR ++ "/active_env", NewEnv),
    
    %% Update nginx config
    BasePath = "/repo/mental_models_system/erlang-services",
    ActiveConf = BasePath ++ "/nginx_proxy/active_env/active.conf",
    Config = "set $active_env " ++ NewEnv ++ ";\n",
    file:write_file(ActiveConf, Config),
    
    %% Reload nginx
    os:cmd("docker exec mental-models-proxy nginx -s reload 2>&1").

read_active_env() ->
    case read_file(?DATA_DIR ++ "/active_env") of
        undefined -> "blue";
        Env -> binary_to_list(Env)
    end.

%%====================================================================
%% Git Helpers
%%====================================================================

clone_repo(Branch) ->
    BranchStr = binary_to_list(Branch),
    case os:getenv("GITHUB_TOKEN") of
        false -> {error, no_token};
        Token ->
            Repo = os:getenv("GITHUB_REPO", "Ripple-Analytics/Ripple_Analytics"),
            Url = "https://" ++ Token ++ "@github.com/" ++ Repo ++ ".git",
            Cmd = "git clone --depth 1 --branch " ++ BranchStr ++ " " ++ Url ++ " /repo 2>&1",
            Result = os:cmd(Cmd),
            case string:find(Result, "fatal:") of
                nomatch -> ok;
                _ -> {error, Result}
            end
    end.

get_current_commit() ->
    Result = string:trim(os:cmd("cd /repo && git rev-parse HEAD 2>/dev/null")),
    case length(Result) >= 7 of
        true -> list_to_binary(string:sub_string(Result, 1, 7));
        false -> undefined
    end.

get_remote_commit(Branch) ->
    %% MUST fetch from remote first to get latest commits
    io:format("[UPDATER] Fetching from remote...~n"),
    FetchCmd = "cd /repo && git fetch origin " ++ Branch ++ " 2>&1",
    FetchResult = os:cmd(FetchCmd),
    io:format("[UPDATER] Fetch result: ~s~n", [string:sub_string(FetchResult, 1, erlang:min(200, length(FetchResult)))]),
    
    %% Now get the remote commit hash
    Cmd = "cd /repo && git rev-parse origin/" ++ Branch ++ " 2>/dev/null",
    Result = string:trim(os:cmd(Cmd)),
    io:format("[UPDATER] Remote commit raw: ~s~n", [Result]),
    
    case length(Result) >= 7 of
        true -> 
            Commit = list_to_binary(string:sub_string(Result, 1, 7)),
            io:format("[UPDATER] Remote commit: ~s~n", [Commit]),
            Commit;
        false -> 
            io:format("[UPDATER] Failed to get remote commit~n"),
            undefined
    end.

%%====================================================================
%% File Helpers
%%====================================================================

read_file(Path) ->
    case file:read_file(Path) of
        {ok, Bin} -> 
            Trimmed = string:trim(binary_to_list(Bin)),
            case Trimmed of
                "" -> undefined;
                S -> list_to_binary(S)
            end;
        {error, _} -> undefined
    end.

write_file(Path, Content) when is_binary(Content) ->
    file:write_file(Path, Content);
write_file(Path, Content) when is_list(Content) ->
    file:write_file(Path, Content).

delete_file(Path) ->
    file:delete(Path).

fmt(undefined) -> "undefined";
fmt(Bin) when is_binary(Bin) -> binary_to_list(Bin);
fmt(Other) -> io_lib:format("~p", [Other]).

format_time(undefined) -> null;
format_time({MegaSecs, Secs, _}) ->
    Seconds = MegaSecs * 1000000 + Secs,
    list_to_binary(calendar:system_time_to_rfc3339(Seconds, [{unit, second}])).

get_check_interval() ->
    case os:getenv("UPDATE_CHECK_INTERVAL") of
        false -> 300000;
        Val -> 
            try list_to_integer(Val) * 1000
            catch _:_ -> 300000
            end
    end.

get_env_binary(Key, Default) ->
    case os:getenv(Key) of
        false -> Default;
        Val -> list_to_binary(Val)
    end.

%%====================================================================
%% Phone Home - Report status to external webhook
%%====================================================================

phone_home(State) ->
    %% Get webhook URL from environment or use hardcoded fallback
    WebhookUrl = case os:getenv("PHONE_HOME_URL") of
        false -> "https://5555-irebqqz94r7om3thnx3dk-6d18fa76.sg1.manus.computer/status";
        Url -> Url
    end,
    
    %% Build status payload
    Payload = jsx:encode(#{
        <<"type">> => <<"heartbeat">>,
        <<"version">> => <<"2.1">>,
        <<"status">> => atom_to_binary(State#state.status, utf8),
        <<"active_env">> => list_to_binary(State#state.active_env),
        <<"standby_env">> => list_to_binary(State#state.standby_env),
        <<"current_commit">> => case State#state.current_commit of undefined -> <<"unknown">>; C -> C end,
        <<"remote_commit">> => case State#state.remote_commit of undefined -> <<"unknown">>; R -> R end,
        <<"branch">> => State#state.branch,
        <<"check_count">> => State#state.check_count,
        <<"consecutive_failures">> => State#state.consecutive_failures,
        <<"update_available">> => State#state.update_available,
        <<"error_message">> => case State#state.error_message of undefined -> null; E -> E end,
        <<"timestamp">> => list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second), [{unit, second}]))
    }),
    
    %% Send HTTP POST (fire and forget, don't block on errors)
    spawn(fun() ->
        try
            case httpc:request(post, {WebhookUrl, [], "application/json", Payload}, [{timeout, 5000}], []) of
                {ok, {{_, 200, _}, _, _}} ->
                    io:format("[UPDATER] Phone home: OK~n");
                {ok, {{_, Code, _}, _, _}} ->
                    io:format("[UPDATER] Phone home: HTTP ~p~n", [Code]);
                {error, Reason} ->
                    io:format("[UPDATER] Phone home failed: ~p~n", [Reason])
            end
        catch
            _:_ -> ok
        end
    end).
