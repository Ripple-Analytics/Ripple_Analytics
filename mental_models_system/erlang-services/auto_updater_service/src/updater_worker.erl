%%%-------------------------------------------------------------------
%%% @doc Updater Worker - Blue-Green Auto-Updater (Main Controller)
%%% 
%%% Uses: git_utils, docker_utils, file_utils, blue_green, phone_home
%%% @end
%%%-------------------------------------------------------------------
-module(updater_worker).
-behaviour(gen_server).

-export([start_link/0, get_status/0, check_updates/0, perform_update/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    status = idle :: idle | checking | updating | error,
    last_check :: undefined | erlang:timestamp(),
    last_update :: undefined | erlang:timestamp(),
    current_commit :: undefined | binary(),
    remote_commit :: undefined | binary(),
    branch = <<"release2">> :: binary(),
    update_available = false :: boolean(),
    active_env = "blue" :: string(),
    standby_env = "green" :: string(),
    error_message :: undefined | binary(),
    check_interval = 300000 :: pos_integer(),
    timer_ref :: undefined | reference(),
    heartbeat_ref :: undefined | reference(),
    check_count = 0 :: non_neg_integer(),
    consecutive_failures = 0 :: non_neg_integer()
}).

-define(MAX_FAILURES, 3).
-define(DATA_DIR, "/data").

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
    gen_server:call(?MODULE, perform_update, 600000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[UPDATER] ========================================~n"),
    io:format("[UPDATER] Blue-Green Auto-Updater v3.1 Starting~n"),
    io:format("[UPDATER] ========================================~n"),
    
    %% Ensure data directory exists
    file_utils:ensure_dir(?DATA_DIR),
    
    %% Read active environment
    ActiveEnv = blue_green:read_active_env(),
    StandbyEnv = case ActiveEnv of 
        "blue" -> "green"; 
        _ -> "blue" 
    end,
    
    io:format("[UPDATER] Active: ~s, Standby: ~s~n", [ActiveEnv, StandbyEnv]),
    
    %% Schedule init and timers
    self() ! init_repo,
    Timer = erlang:send_after(10000, self(), check_timer),
    Heartbeat = erlang:send_after(60000, self(), heartbeat),
    
    {ok, #state{
        timer_ref = Timer, 
        heartbeat_ref = Heartbeat,
        active_env = ActiveEnv, 
        standby_env = StandbyEnv
    }}.

handle_call(get_status, _From, State) ->
    {reply, {ok, build_status(State)}, State};

handle_call(check_updates, _From, State) ->
    NewState = do_check(State),
    {reply, {ok, #{update_available => NewState#state.update_available}}, NewState};

handle_call(perform_update, _From, State) ->
    NewState = do_update(State),
    {reply, ok, NewState};

handle_call(_Request, _From, State) -> 
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) -> 
    {noreply, State}.

handle_info(init_repo, State) ->
    io:format("[UPDATER] Initializing repository state~n"),
    Commit = git_utils:get_current_commit(),
    io:format("[UPDATER] Current commit: ~s~n", [file_utils:fmt(Commit)]),
    {noreply, State#state{current_commit = Commit, status = idle}};

handle_info(heartbeat, State) ->
    %% CRITICAL: Always reschedule heartbeat FIRST
    Ref = erlang:send_after(60000, self(), heartbeat),
    
    io:format("[UPDATER] HEARTBEAT | Checks: ~p | Failures: ~p | Active: ~s~n",
              [State#state.check_count, State#state.consecutive_failures, 
               State#state.active_env]),
    
    %% Report status to external webhook
    phone_home:report(build_status(State)),
    
    {noreply, State#state{heartbeat_ref = Ref}};

handle_info(check_timer, State) ->
    %% CRITICAL: Always reschedule timer FIRST
    Ref = erlang:send_after(State#state.check_interval, self(), check_timer),
    
    Count = State#state.check_count + 1,
    io:format("[UPDATER] ========================================~n"),
    io:format("[UPDATER] CHECK #~p~n", [Count]),
    io:format("[UPDATER] ========================================~n"),
    
    State2 = do_check(State#state{check_count = Count}),
    
    State3 = case State2#state.update_available of
        true -> 
            io:format("[UPDATER] Update available, starting deployment~n"),
            do_update(State2);
        false -> 
            io:format("[UPDATER] No update available~n"),
            State2
    end,
    
    {noreply, State3#state{timer_ref = Ref}};

handle_info(_Info, State) -> 
    {noreply, State}.

terminate(_Reason, _State) -> 
    ok.

%%====================================================================
%% Core Logic
%%====================================================================

do_check(State) ->
    Branch = binary_to_list(State#state.branch),
    
    %% Get current and remote commits
    Current = git_utils:get_current_commit(),
    Remote = git_utils:get_remote_commit(Branch),
    
    %% Read last failed commit to avoid retry loops
    LastFailed = file_utils:read(?DATA_DIR ++ "/last_failed_commit"),
    LastProcessed = file_utils:read(?DATA_DIR ++ "/last_processed"),
    
    io:format("[UPDATER] Current: ~s~n", [file_utils:fmt(Current)]),
    io:format("[UPDATER] Remote: ~s~n", [file_utils:fmt(Remote)]),
    io:format("[UPDATER] LastFailed: ~s~n", [file_utils:fmt(LastFailed)]),
    io:format("[UPDATER] LastProcessed: ~s~n", [file_utils:fmt(LastProcessed)]),
    
    %% Determine if update is available
    Available = is_update_available(Current, Remote, LastFailed, 
                                    State#state.consecutive_failures),
    
    io:format("[UPDATER] Update available: ~p~n", [Available]),
    
    State#state{
        current_commit = Current, 
        remote_commit = Remote,
        update_available = Available, 
        last_check = erlang:timestamp()
    }.

is_update_available(Current, Remote, LastFailed, Failures) ->
    %% Check all conditions for update
    (Remote =/= undefined) andalso
    (Current =/= Remote) andalso
    (Remote =/= LastFailed) andalso
    (Failures < ?MAX_FAILURES).

do_update(State) ->
    Branch = binary_to_list(State#state.branch),
    Standby = State#state.standby_env,
    Active = State#state.active_env,
    
    io:format("[UPDATER] ========================================~n"),
    io:format("[UPDATER] DEPLOYING TO STANDBY: ~s~n", [Standby]),
    io:format("[UPDATER] ========================================~n"),
    
    %% Reset to latest code
    git_utils:reset_hard(Branch),
    NewCommit = git_utils:get_current_commit(),
    io:format("[UPDATER] Pulled commit: ~s~n", [file_utils:fmt(NewCommit)]),
    
    %% Get services to build
    Services = blue_green:get_standby_services(Standby),
    io:format("[UPDATER] Building services: ~p~n", [Services]),
    
    %% Build, start, health check, switch
    case blue_green:build_standby(Services) of
        ok ->
            io:format("[UPDATER] Build successful, starting services~n"),
            blue_green:start_standby(Services),
            
            %% Wait for services to start
            io:format("[UPDATER] Waiting for services to start...~n"),
            timer:sleep(15000),
            
            %% Health check
            io:format("[UPDATER] Running health check~n"),
            case blue_green:check_standby_health(Standby) of
                true ->
                    io:format("[UPDATER] Health check PASSED~n"),
                    blue_green:switch_traffic(Standby),
                    
                    %% Record success
                    file_utils:write(?DATA_DIR ++ "/last_processed", NewCommit),
                    file_utils:delete(?DATA_DIR ++ "/last_failed_commit"),
                    
                    io:format("[UPDATER] ========================================~n"),
                    io:format("[UPDATER] SUCCESS! Now serving from: ~s~n", [Standby]),
                    io:format("[UPDATER] ========================================~n"),
                    
                    State#state{
                        status = idle, 
                        current_commit = NewCommit,
                        active_env = Standby, 
                        standby_env = Active,
                        consecutive_failures = 0, 
                        update_available = false,
                        last_update = erlang:timestamp()
                    };
                    
                false ->
                    handle_failure(State, NewCommit, "Health check failed")
            end;
            
        {error, Reason} ->
            handle_failure(State, NewCommit, Reason)
    end.

handle_failure(State, Commit, Reason) ->
    Failures = State#state.consecutive_failures + 1,
    
    io:format("[UPDATER] ========================================~n"),
    io:format("[UPDATER] DEPLOYMENT FAILED~n"),
    io:format("[UPDATER] Reason: ~s~n", [Reason]),
    io:format("[UPDATER] Failure count: ~p/~p~n", [Failures, ?MAX_FAILURES]),
    io:format("[UPDATER] ========================================~n"),
    
    %% Record failure
    file_utils:write(?DATA_DIR ++ "/last_failed_commit", Commit),
    
    %% Revert to previous commit
    git_utils:revert_commit(),
    
    ErrorMsg = case is_list(Reason) of
        true -> list_to_binary(Reason);
        false -> <<"unknown error">>
    end,
    
    State#state{
        status = error, 
        error_message = ErrorMsg,
        consecutive_failures = Failures, 
        update_available = false
    }.

%%====================================================================
%% Helpers
%%====================================================================

build_status(State) ->
    #{
        status => State#state.status,
        active_env => list_to_binary(State#state.active_env),
        standby_env => list_to_binary(State#state.standby_env),
        current_commit => State#state.current_commit,
        remote_commit => State#state.remote_commit,
        update_available => State#state.update_available,
        check_count => State#state.check_count,
        failures => State#state.consecutive_failures,
        error_message => State#state.error_message,
        last_check => file_utils:format_time(State#state.last_check),
        last_update => file_utils:format_time(State#state.last_update)
    }.
