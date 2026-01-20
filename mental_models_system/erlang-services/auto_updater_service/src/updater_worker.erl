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
    status = idle, last_check, last_update, current_commit, remote_commit,
    branch = <<"release2">>, update_available = false,
    active_env = "blue", standby_env = "green",
    error_message, check_interval = 300000, timer_ref, heartbeat_ref,
    check_count = 0, consecutive_failures = 0
}).

-define(MAX_FAILURES, 3).
-define(DATA_DIR, "/data").

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
get_status() -> gen_server:call(?MODULE, get_status).
check_updates() -> gen_server:call(?MODULE, check_updates, 60000).
perform_update() -> gen_server:call(?MODULE, perform_update, 600000).

init([]) ->
    io:format("[UPDATER] Starting Blue-Green Auto-Updater v3.0~n"),
    file_utils:ensure_dir(?DATA_DIR),
    ActiveEnv = blue_green:read_active_env(),
    StandbyEnv = case ActiveEnv of "blue" -> "green"; _ -> "blue" end,
    self() ! init_repo,
    Timer = erlang:send_after(5000, self(), check_timer),
    Heartbeat = erlang:send_after(60000, self(), heartbeat),
    {ok, #state{timer_ref = Timer, heartbeat_ref = Heartbeat,
                active_env = ActiveEnv, standby_env = StandbyEnv}}.

handle_call(get_status, _From, S) ->
    {reply, {ok, build_status(S)}, S};
handle_call(check_updates, _From, S) ->
    NewS = do_check(S),
    {reply, {ok, #{update_available => NewS#state.update_available}}, NewS};
handle_call(perform_update, _From, S) ->
    NewS = do_update(S),
    {reply, ok, NewS};
handle_call(_, _From, S) -> {reply, ok, S}.

handle_cast(_, S) -> {noreply, S}.

handle_info(init_repo, S) ->
    Commit = git_utils:get_current_commit(),
    {noreply, S#state{current_commit = Commit, status = idle}};

handle_info(heartbeat, S) ->
    Ref = erlang:send_after(60000, self(), heartbeat),
    io:format("[UPDATER] HEARTBEAT | Checks: ~p | Failures: ~p~n",
              [S#state.check_count, S#state.consecutive_failures]),
    phone_home:report(build_status(S)),
    {noreply, S#state{heartbeat_ref = Ref}};

handle_info(check_timer, S) ->
    Ref = erlang:send_after(S#state.check_interval, self(), check_timer),
    Count = S#state.check_count + 1,
    io:format("[UPDATER] Check #~p~n", [Count]),
    S2 = do_check(S#state{check_count = Count}),
    S3 = case S2#state.update_available of
        true -> do_update(S2);
        false -> S2
    end,
    {noreply, S3#state{timer_ref = Ref}};

handle_info(_, S) -> {noreply, S}.

terminate(_, _) -> ok.

%% === Core Logic ===

do_check(S) ->
    Branch = binary_to_list(S#state.branch),
    Current = git_utils:get_current_commit(),
    Remote = git_utils:get_remote_commit(Branch),
    LastFailed = file_utils:read(?DATA_DIR ++ "/last_failed_commit"),
    io:format("[UPDATER] Current: ~s, Remote: ~s~n", 
              [file_utils:fmt(Current), file_utils:fmt(Remote)]),
    Available = (Remote =/= undefined) andalso (Current =/= Remote) andalso
                (Remote =/= LastFailed) andalso 
                (S#state.consecutive_failures < ?MAX_FAILURES),
    S#state{current_commit = Current, remote_commit = Remote,
            update_available = Available, last_check = erlang:timestamp()}.

do_update(S) ->
    Branch = binary_to_list(S#state.branch),
    Standby = S#state.standby_env,
    Active = S#state.active_env,
    io:format("[UPDATER] Deploying to standby: ~s~n", [Standby]),
    
    git_utils:reset_hard(Branch),
    NewCommit = git_utils:get_current_commit(),
    Services = blue_green:get_standby_services(Standby),
    
    case blue_green:build_standby(Services) of
        ok ->
            blue_green:start_standby(Services),
            timer:sleep(10000),
            case blue_green:check_standby_health(Standby) of
                true ->
                    blue_green:switch_traffic(Standby),
                    file_utils:write(?DATA_DIR ++ "/last_processed", NewCommit),
                    file_utils:delete(?DATA_DIR ++ "/last_failed_commit"),
                    io:format("[UPDATER] SUCCESS! Now on: ~s~n", [Standby]),
                    S#state{status = idle, current_commit = NewCommit,
                            active_env = Standby, standby_env = Active,
                            consecutive_failures = 0, update_available = false};
                false ->
                    handle_failure(S, NewCommit, "Health check failed")
            end;
        {error, Reason} ->
            handle_failure(S, NewCommit, Reason)
    end.

handle_failure(S, Commit, Reason) ->
    Failures = S#state.consecutive_failures + 1,
    io:format("[UPDATER] FAILED: ~s (failure #~p)~n", [Reason, Failures]),
    file_utils:write(?DATA_DIR ++ "/last_failed_commit", Commit),
    git_utils:revert_commit(),
    S#state{status = error, error_message = list_to_binary(Reason),
            consecutive_failures = Failures, update_available = false}.

build_status(S) ->
    #{status => S#state.status, active_env => list_to_binary(S#state.active_env),
      current_commit => S#state.current_commit, remote_commit => S#state.remote_commit,
      check_count => S#state.check_count, failures => S#state.consecutive_failures}.
