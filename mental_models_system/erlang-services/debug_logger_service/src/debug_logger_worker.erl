%%%-------------------------------------------------------------------
%%% @doc Debug Logger Worker - Bulletproof logging to GitHub
%%% 
%%% STARTS FIRST - Immediately pushes "started" to GitHub
%%% All functions wrapped in try/catch - NEVER crashes
%%% @end
%%%-------------------------------------------------------------------
-module(debug_logger_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([log/3, log/4, error/3, error/4, startup/1, heartbeat/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(REPO_PATH, "/repo").
-define(LOG_BASE, "/repo/mental_models_system/erlang-services/debug_logs").
-define(HEARTBEAT_INTERVAL, 60000).  %% 1 minute

-record(state, {
    env :: string(),
    start_time :: erlang:timestamp(),
    log_count = 0 :: non_neg_integer()
}).

%%====================================================================
%% API - All wrapped in try/catch, NEVER crash
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Log message (async, never blocks)
log(Service, Category, Message) ->
    safe_cast({log, "blue", Service, Category, Message, "INFO"}).

log(Env, Service, Category, Message) ->
    safe_cast({log, Env, Service, Category, Message, "INFO"}).

%% Log error (async, pushes immediately)
error(Service, Category, Message) ->
    safe_cast({log, "blue", Service, Category, Message, "ERROR"}).

error(Env, Service, Category, Message) ->
    safe_cast({log, Env, Service, Category, Message, "ERROR"}).

%% Log startup (sync, waits for push)
startup(Service) ->
    safe_call({startup, Service}).

%% Trigger heartbeat
heartbeat() ->
    safe_cast(heartbeat).

%%====================================================================
%% Safe wrappers - NEVER crash
%%====================================================================

safe_cast(Msg) ->
    try
        gen_server:cast(?MODULE, Msg)
    catch
        _:_ -> ok
    end.

safe_call(Msg) ->
    try
        gen_server:call(?MODULE, Msg, 30000)
    catch
        _:_ -> {error, logger_unavailable}
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[DEBUG_LOGGER] Initializing...~n"),
    
    %% Read deployment environment from env var (default to blue)
    Env = case os:getenv("DEPLOYMENT_ENV") of
        false -> "blue";
        E -> E
    end,
    io:format("[DEBUG_LOGGER] Environment: ~s~n", [Env]),
    
    %% IMMEDIATELY log startup and push to GitHub
    self() ! immediate_startup_log,
    
    %% Schedule heartbeat
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat_timer),
    
    {ok, #state{env = Env, start_time = erlang:timestamp()}}.

handle_call({startup, Service}, _From, State) ->
    %% Synchronous startup log - waits for push
    try
        do_log(State#state.env, Service, "startup", 
               "Service started at " ++ format_timestamp(), "INFO"),
        do_push_sync()
    catch
        _:_ -> ok
    end,
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({log, Env, Service, Category, Message, Level}, State) ->
    try
        do_log(Env, Service, Category, Message, Level),
        case Level of
            "ERROR" -> spawn(fun() -> do_push_async() end);
            _ -> ok
        end
    catch
        _:_ -> ok
    end,
    {noreply, State#state{log_count = State#state.log_count + 1}};

handle_cast(heartbeat, State) ->
    try
        do_heartbeat(State)
    catch
        _:_ -> ok
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(immediate_startup_log, State) ->
    io:format("[DEBUG_LOGGER] Pushing immediate startup log to GitHub...~n"),
    try
        %% Get system info
        Hostname = safe_cmd("hostname"),
        DockerPs = safe_cmd("docker ps -a --format 'table {{.Names}}\\t{{.Status}}' 2>&1"),
        GitStatus = safe_cmd("cd " ++ ?REPO_PATH ++ " && git log --oneline -1 2>&1"),
        
        StartupMsg = io_lib:format(
            "DEBUG LOGGER STARTED~n"
            "====================~n"
            "Timestamp: ~s~n"
            "Hostname: ~s~n"
            "Erlang OTP: ~s~n~n"
            "Git Status:~n~s~n~n"
            "Docker Containers:~n~s~n",
            [format_timestamp(), Hostname, erlang:system_info(otp_release),
             GitStatus, DockerPs]),
        
        do_log("blue", "debug_logger", "startup", lists:flatten(StartupMsg), "INFO"),
        do_push_sync(),
        io:format("[DEBUG_LOGGER] Startup log pushed to GitHub!~n")
    catch
        Class:Reason ->
            io:format("[DEBUG_LOGGER] Startup log failed: ~p:~p~n", [Class, Reason])
    end,
    {noreply, State};

handle_info(heartbeat_timer, State) ->
    %% Reschedule first
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat_timer),
    
    try
        do_heartbeat(State)
    catch
        _:_ -> ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%====================================================================
%% Internal Functions - All wrapped in try/catch
%%====================================================================

do_log(Env, Service, Category, Message, Level) ->
    try
        %% Ensure directories exist
        LogsDir = ?LOG_BASE ++ "/" ++ Env ++ "_logs/" ++ safe_name(Service),
        StateDir = ?LOG_BASE ++ "/" ++ Env ++ "/" ++ safe_name(Service),
        
        filelib:ensure_dir(LogsDir ++ "/"),
        filelib:ensure_dir(StateDir ++ "/"),
        
        %% Build log content
        Timestamp = format_timestamp(),
        LogFile = LogsDir ++ "/" ++ Category ++ "_" ++ Timestamp ++ ".log",
        LatestFile = LogsDir ++ "/" ++ Category ++ "_latest.log",
        
        Content = io_lib:format(
            "================================================================================~n"
            "~s | ~s | ~s | ~s~n"
            "Timestamp: ~s~n"
            "================================================================================~n~n"
            "~s~n~n"
            "================================================================================~n",
            [Level, Env, Service, Category, Timestamp, Message]),
        
        FlatContent = lists:flatten(Content),
        
        %% Write files
        file:write_file(LogFile, FlatContent),
        file:write_file(LatestFile, FlatContent),
        
        %% Append to state log
        StateFile = StateDir ++ "/state.log",
        StateLine = io_lib:format("~s | ~s | ~s | ~s~n", 
                                  [Timestamp, Level, Category, truncate(Message, 100)]),
        file:write_file(StateFile, lists:flatten(StateLine), [append]),
        
        io:format("[DEBUG_LOGGER] Logged: ~s/~s/~s~n", [Env, Service, Category])
    catch
        _:_ -> ok
    end.

do_heartbeat(State) ->
    Uptime = timer:now_diff(erlang:timestamp(), State#state.start_time) div 1000000,
    DockerPs = safe_cmd("docker ps -a --format 'table {{.Names}}\\t{{.Status}}' 2>&1"),
    
    Msg = io_lib:format(
        "HEARTBEAT~n"
        "=========~n"
        "Uptime: ~p seconds~n"
        "Logs written: ~p~n~n"
        "Docker Status:~n~s~n",
        [Uptime, State#state.log_count, DockerPs]),
    
    do_log(State#state.env, "debug_logger", "heartbeat", lists:flatten(Msg), "INFO"),
    spawn(fun() -> do_push_async() end).

do_push_sync() ->
    try
        %% Configure git
        os:cmd("cd " ++ ?REPO_PATH ++ " && git config user.email 'debug@mental-models.local' 2>&1"),
        os:cmd("cd " ++ ?REPO_PATH ++ " && git config user.name 'Debug Logger' 2>&1"),
        
        %% Add logs
        os:cmd("cd " ++ ?REPO_PATH ++ " && git add mental_models_system/erlang-services/debug_logs/ 2>&1"),
        
        %% Commit
        CommitMsg = "debug: " ++ format_timestamp(),
        os:cmd("cd " ++ ?REPO_PATH ++ " && git commit -m '" ++ CommitMsg ++ "' 2>&1"),
        
        %% Push
        Result = os:cmd("cd " ++ ?REPO_PATH ++ " && git push origin release2 2>&1"),
        io:format("[DEBUG_LOGGER] Push result: ~s~n", [truncate(Result, 200)]),
        ok
    catch
        _:_ -> {error, push_failed}
    end.

do_push_async() ->
    timer:sleep(2000),  %% Small delay to batch
    do_push_sync().

safe_cmd(Cmd) ->
    try
        string:trim(os:cmd(Cmd))
    catch
        _:_ -> "command failed"
    end.

safe_name(Name) ->
    try
        S1 = string:replace(Name, "-", "_", all),
        S2 = string:replace(S1, " ", "_", all),
        lists:flatten(S2)
    catch
        _:_ -> "unknown"
    end.

format_timestamp() ->
    try
        {{Y, M, D}, {H, Mi, S}} = calendar:local_time(),
        lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B_~2..0B-~2..0B-~2..0B",
                                    [Y, M, D, H, Mi, S]))
    catch
        _:_ -> "unknown_time"
    end.

truncate(Str, Max) when is_list(Str) ->
    try
        case length(Str) > Max of
            true -> string:slice(Str, 0, Max) ++ "...";
            false -> Str
        end
    catch
        _:_ -> "..."
    end;
truncate(_, _) -> "...".
