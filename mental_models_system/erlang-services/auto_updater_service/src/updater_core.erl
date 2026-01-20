%%%-------------------------------------------------------------------
%%% @doc Updater Core - Main Update Logic
%%% 
%%% Contains the core update logic for blue-green deployments.
%%% @end
%%%-------------------------------------------------------------------
-module(updater_core).

-export([perform_update/1, handle_failure/3]).

-define(DATA_DIR, "/data").

%%====================================================================
%% Perform Update
%%====================================================================

perform_update(State) ->
    RemoteCommit = maps:get(remote_commit, State),
    StandbyEnv = maps:get(standby_env, State),
    ActiveEnv = maps:get(active_env, State),
    Branch = binary_to_list(maps:get(branch, State)),
    
    io:format("[UPDATER] ========================================~n"),
    io:format("[UPDATER] DEPLOYING TO STANDBY: ~s~n", [StandbyEnv]),
    io:format("[UPDATER] Active (~s) continues serving traffic~n", [ActiveEnv]),
    io:format("[UPDATER] ========================================~n"),
    
    os:cmd("cd /repo && git reset --hard origin/" ++ Branch),
    NewCommit = updater_utils:get_current_commit(),
    io:format("[UPDATER] Pulled commit: ~s~n", [updater_utils:fmt(NewCommit)]),
    
    io:format("[UPDATER] Running pre-build syntax check...~n"),
    PreBuildResult = os:cmd("cd /repo/mental_models_system/erlang-services && if [ -f scripts/fix_erlang_syntax.sh ]; then bash scripts/fix_erlang_syntax.sh 2>&1; else echo 'Syntax script not found, skipping'; fi"),
    PreBuildLen = case is_list(PreBuildResult) of true -> length(PreBuildResult); false -> 0 end,
    io:format("[UPDATER] Pre-build result: ~s~n", [case PreBuildLen of 0 -> "(no output)"; N when N > 500 -> string:sub_string(PreBuildResult, 1, 500); _ -> PreBuildResult end]),
    
    StandbyServices = updater_blue_green:get_standby_services(StandbyEnv),
    BuildResult = updater_blue_green:build_standby(StandbyServices),
    
    case BuildResult of
        ok ->
            updater_blue_green:start_standby(StandbyServices),
            timer:sleep(15000),
            
            case updater_blue_green:check_health(StandbyEnv) of
                true ->
                    io:format("[UPDATER] Standby healthy, switching traffic~n"),
                    updater_blue_green:switch_traffic(StandbyEnv),
                    
                    updater_utils:write_file(?DATA_DIR ++ "/last_processed_commit", NewCommit),
                    updater_utils:delete_file(?DATA_DIR ++ "/last_failed_commit"),
                    
                    io:format("[UPDATER] ========================================~n"),
                    io:format("[UPDATER] SUCCESS! Traffic now on: ~s~n", [StandbyEnv]),
                    io:format("[UPDATER] ========================================~n"),
                    
                    maps:merge(State, #{
                        status => idle,
                        last_update => erlang:timestamp(),
                        current_commit => NewCommit,
                        update_available => false,
                        active_env => StandbyEnv,
                        standby_env => ActiveEnv,
                        consecutive_failures => 0,
                        last_failed_commit => undefined
                    });
                false ->
                    handle_failure(State, NewCommit, "Health check failed")
            end;
        {error, Reason} ->
            handle_failure(State, NewCommit, Reason)
    end.

%%====================================================================
%% Failure Handling
%%====================================================================

handle_failure(State, FailedCommit, Reason) ->
    NewFailures = maps:get(consecutive_failures, State, 0) + 1,
    io:format("[UPDATER] FAILED: ~s (failure #~p)~n", [Reason, NewFailures]),
    
    updater_utils:write_file(?DATA_DIR ++ "/last_failed_commit", FailedCommit),
    os:cmd("cd /repo && git reset --hard HEAD~1 2>&1"),
    
    io:format("[UPDATER] Active (~s) continues serving~n", [maps:get(active_env, State)]),
    
    maps:merge(State, #{
        status => error,
        error_message => list_to_binary(Reason),
        consecutive_failures => NewFailures,
        last_failed_commit => FailedCommit,
        update_available => false
    }).
