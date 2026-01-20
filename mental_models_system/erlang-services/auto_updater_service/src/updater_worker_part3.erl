%%%-------------------------------------------------------------------
%%% @doc updater_worker Helper Module - Part 3
%%% @end
%%%-------------------------------------------------------------------
-module(updater_worker_part3).

-export([do_perform_update/1, handle_failure/3, get_standby_services/1, build_standby/1, build_services/3]).

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
    
    %% Step 1.5: Run pre-build syntax check and auto-fix (optional - don't fail if missing)
    io:format("[UPDATER] Running pre-build syntax check...~n"),
    PreBuildResult = os:cmd("cd /repo/mental_models_system/erlang-services && if [ -f scripts/fix_erlang_syntax.sh ]; then bash scripts/fix_erlang_syntax.sh 2>&1; else echo 'Syntax script not found, skipping'; fi"),
    PreBuildLen = case is_list(PreBuildResult) of true -> length(PreBuildResult); false -> 0 end,
    io:format("[UPDATER] Pre-build result: ~s~n", [case PreBuildLen of 0 -> "(no output)"; N when N > 500 -> string:sub_string(PreBuildResult, 1, 500); _ -> PreBuildResult end]),
    
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
