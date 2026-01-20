%%%-------------------------------------------------------------------
%%% @doc updater_worker Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(updater_worker_part2).

-export([handle_info/2, handle_info/2, terminate/2, do_init_repo/1, do_check_updates/1]).

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

