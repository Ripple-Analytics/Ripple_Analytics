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

-define(CLONE_PATH, "/tmp/debug_repo").
-define(LOG_BASE, "/tmp/debug_repo/mental_models_system/erlang-services/debug_logs").
-define(HEARTBEAT_INTERVAL, 60000).

-record(state, {
    env :: string(),
    start_time :: erlang:timestamp(),
    log_count = 0 :: non_neg_integer()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

log(Service, Category, Message) ->
    safe_cast({log, "blue", Service, Category, Message, "INFO"}).

log(Env, Service, Category, Message) ->
    safe_cast({log, Env, Service, Category, Message, "INFO"}).

error(Service, Category, Message) ->
    safe_cast({log, "blue", Service, Category, Message, "ERROR"}).

error(Env, Service, Category, Message) ->
    safe_cast({log, Env, Service, Category, Message, "ERROR"}).

startup(Service) ->
    safe_call({startup, Service}).

heartbeat() ->
    safe_cast(heartbeat).

%%====================================================================
%% Safe wrappers
%%====================================================================

safe_cast(Msg) ->
    try gen_server:cast(?MODULE, Msg) catch _:_ -> ok end.

safe_call(Msg) ->
    try gen_server:call(?MODULE, Msg, 30000) catch _:_ -> {error, logger_unavailable} end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[DEBUG_LOGGER] ========================================~n"),
    io:format("[DEBUG_LOGGER] Debug Logger Service Starting~n"),
    io:format("[DEBUG_LOGGER] ========================================~n"),
    io:format("[DEBUG_LOGGER] Initializing...~n"),
    
    Env = case os:getenv("DEPLOYMENT_ENV") of
        false -> "blue";
        E -> E
    end,
    io:format("[DEBUG_LOGGER] Environment: ~s~n", [Env]),
    
    self() ! immediate_startup_log,
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat_timer),
    
    {ok, #state{env = Env, start_time = erlang:timestamp()}}.

handle_call({startup, Service}, _From, State) ->
    try
        do_log(State#state.env, Service, "startup", 
               "Service started at " ++ format_timestamp(), "INFO"),
        do_push_sync()
    catch _:_ -> ok end,
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
    catch _:_ -> ok end,
    {noreply, State#state{log_count = State#state.log_count + 1}};

handle_cast(heartbeat, State) ->
    try do_heartbeat(State) catch _:_ -> ok end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(immediate_startup_log, State) ->
    io:format("[DEBUG_LOGGER] Pushing immediate startup log to GitHub...~n"),
    try
        StartupMsg = io_lib:format(
            "DEBUG LOGGER STARTED~n"
            "====================~n"
            "Timestamp: ~s~n"
            "Erlang OTP: ~s~n",
            [format_timestamp(), erlang:system_info(otp_release)]),
        
        do_log("blue", "debug_logger", "startup", lists:flatten(StartupMsg), "INFO"),
        do_push_sync(),
        io:format("[DEBUG_LOGGER] Startup log pushed to GitHub!~n")
    catch
        Class:Reason ->
            io:format("[DEBUG_LOGGER] Startup log failed: ~p:~p~n", [Class, Reason])
    end,
    {noreply, State};

handle_info(heartbeat_timer, State) ->
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat_timer),
    try do_heartbeat(State) catch _:_ -> ok end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

do_log(Env, Service, Category, Message, Level) ->
    try
        LogsDir = ?LOG_BASE ++ "/" ++ Env ++ "_logs/" ++ safe_name(Service),
        filelib:ensure_dir(LogsDir ++ "/"),
        
        Timestamp = format_timestamp(),
        LogFile = LogsDir ++ "/" ++ Category ++ "_" ++ Timestamp ++ ".log",
        
        Content = io_lib:format(
            "~s | ~s | ~s | ~s~n"
            "Timestamp: ~s~n~n"
            "~s~n",
            [Level, Env, Service, Category, Timestamp, Message]),
        
        file:write_file(LogFile, lists:flatten(Content)),
        io:format("[DEBUG_LOGGER] Logged: ~s/~s/~s~n", [Env, Service, Category])
    catch _:_ -> ok end.

do_heartbeat(State) ->
    Uptime = timer:now_diff(erlang:timestamp(), State#state.start_time) div 1000000,
    Msg = io_lib:format("HEARTBEAT - Uptime: ~p seconds, Logs: ~p~n",
                        [Uptime, State#state.log_count]),
    do_log(State#state.env, "debug_logger", "heartbeat", lists:flatten(Msg), "INFO"),
    spawn(fun() -> do_push_async() end).

do_push_sync() ->
    try
        %% Get token
        RawToken = case os:getenv("GITHUB_TOKEN") of
            false -> "";
            T -> T
        end,
        Token = string:trim(RawToken, both, " \t\n\r"),
        io:format("[DEBUG_LOGGER] Token length: ~p~n", [length(Token)]),
        
        case length(Token) > 30 of
            true ->
                %% Remove old clone
                os:cmd("rm -rf " ++ ?CLONE_PATH ++ " 2>&1"),
                
                %% Clone fresh with token in URL
                CloneUrl = "https://" ++ Token ++ "@github.com/Ripple-Analytics/Ripple_Analytics.git",
                CloneCmd = "git clone --depth 1 --branch release2 " ++ CloneUrl ++ " " ++ ?CLONE_PATH ++ " 2>&1",
                CloneResult = os:cmd(CloneCmd),
                io:format("[DEBUG_LOGGER] Clone result: ~s~n", [truncate(CloneResult, 100)]),
                
                %% Configure git
                os:cmd("cd " ++ ?CLONE_PATH ++ " && git config user.email 'debug@mental-models.local' 2>&1"),
                os:cmd("cd " ++ ?CLONE_PATH ++ " && git config user.name 'Debug Logger' 2>&1"),
                
                %% Ensure log directory exists
                filelib:ensure_dir(?LOG_BASE ++ "/"),
                os:cmd("mkdir -p " ++ ?LOG_BASE ++ " 2>&1"),
                
                %% Write a simple test file
                TestFile = ?LOG_BASE ++ "/test_" ++ format_timestamp() ++ ".log",
                file:write_file(TestFile, "Debug logger test at " ++ format_timestamp() ++ "\n"),
                
                %% Add, commit, push
                os:cmd("cd " ++ ?CLONE_PATH ++ " && git add -A 2>&1"),
                CommitMsg = "debug: " ++ format_timestamp(),
                CommitResult = os:cmd("cd " ++ ?CLONE_PATH ++ " && git commit -m '" ++ CommitMsg ++ "' 2>&1"),
                io:format("[DEBUG_LOGGER] Commit result: ~s~n", [truncate(CommitResult, 100)]),
                
                PushResult = os:cmd("cd " ++ ?CLONE_PATH ++ " && git push origin release2 2>&1"),
                io:format("[DEBUG_LOGGER] Push result: ~s~n", [truncate(PushResult, 200)]),
                ok;
            false ->
                io:format("[DEBUG_LOGGER] No valid token, skipping push~n"),
                ok
        end
    catch _:_ -> {error, push_failed} end.

do_push_async() ->
    timer:sleep(2000),
    do_push_sync().

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
    try
        case length(Str) > Max of
            true -> string:slice(Str, 0, Max) ++ "...";
            false -> Str
        end
    catch _:_ -> "..." end;
truncate(_, _) -> "...".
