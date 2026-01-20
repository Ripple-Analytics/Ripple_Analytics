%%%-------------------------------------------------------------------
%%% @doc github_worker Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(github_worker_part2).

-export([try_https_public/2, do_check_updates/1, do_fetch_latest/1, get_commit/1, get_remote_commit/2, run_git/1, read_last_processed_commit/1, save_last_processed_commit/2, save_last_processed_commit/2]).

try_https_public(RepoPath, Branch) ->
    case os:getenv("GITHUB_REPO_URL") of
        false -> {error, <<"No GitHub URL configured">>};
        Url ->
            Cmd = "git clone --depth 1 --branch " ++ Branch ++ " " ++ Url ++ " " ++ RepoPath ++ " 2>&1",
            case run_git(Cmd) of
                ok -> {ok, https_public};
                _ -> {error, <<"All GitHub methods failed">>}
            end
    end.

do_check_updates(State) ->
    RepoPath = State#state.repo_path,
    Branch = binary_to_list(State#state.branch),
    
    os:cmd("cd " ++ RepoPath ++ " && git fetch origin " ++ Branch ++ " --depth 1 2>&1"),
    
    Current = get_commit(RepoPath),
    Remote = get_remote_commit(RepoPath, Branch),
    
    %% Check last processed commit to prevent loops
    LastProcessed = read_last_processed_commit(RepoPath),
    
    UpdateAvailable = (Current =/= undefined) andalso 
                      (Remote =/= undefined) andalso 
                      (Current =/= Remote) andalso
                      (Remote =/= LastProcessed),
    
    io:format("[GITHUB-WORKER] Current: ~p, Remote: ~p, LastProcessed: ~p, Update: ~p~n",
              [Current, Remote, LastProcessed, UpdateAvailable]),
    
    Now = erlang:timestamp(),
    State#state{
        last_check = Now,
        current_commit = Current,
        remote_commit = Remote,
        update_available = UpdateAvailable,
        status = idle
    }.

do_fetch_latest(State) ->
    RepoPath = State#state.repo_path,
    Branch = binary_to_list(State#state.branch),
    
    Cmd = "cd " ++ RepoPath ++ " && git reset --hard origin/" ++ Branch ++ " 2>&1",
    os:cmd(Cmd),
    
    NewCommit = get_commit(RepoPath),
    
    %% Save as processed to prevent loops
    save_last_processed_commit(RepoPath, NewCommit),
    
    State#state{current_commit = NewCommit, update_available = false, status = idle}.

get_commit(RepoPath) ->
    Result = os:cmd("cd " ++ RepoPath ++ " && git rev-parse HEAD 2>/dev/null"),
    case string:trim(Result) of
        "" -> undefined;
        C when length(C) >= 7 -> list_to_binary(string:sub_string(C, 1, 7));
        _ -> undefined
    end.

get_remote_commit(RepoPath, Branch) ->
    %% First ensure we have the latest refs
    FetchCmd = "cd " ++ RepoPath ++ " && git fetch origin " ++ Branch ++ " 2>&1",
    _FetchResult = os:cmd(FetchCmd),
    
    %% Now get the remote commit
    Cmd = "cd " ++ RepoPath ++ " && git rev-parse origin/" ++ Branch ++ " 2>/dev/null",
    Result = os:cmd(Cmd),
    TrimmedResult = string:trim(Result),
    
    %% Debug logging
    io:format("[GITHUB-WORKER] get_remote_commit result: ~s~n", [TrimmedResult]),
    
    case TrimmedResult of
        "" -> 
            %% Try FETCH_HEAD as fallback
            FallbackCmd = "cd " ++ RepoPath ++ " && git rev-parse FETCH_HEAD 2>/dev/null",
            FallbackResult = string:trim(os:cmd(FallbackCmd)),
            case FallbackResult of
                "" -> undefined;
                FB when length(FB) >= 7 -> list_to_binary(string:sub_string(FB, 1, 7));
                _ -> undefined
            end;
        C when length(C) >= 7 -> 
            list_to_binary(string:sub_string(C, 1, 7));
        _ -> 
            undefined
    end.

run_git(Cmd) ->
    Result = os:cmd(Cmd),
    case string:find(Result, "fatal:") of
        nomatch -> ok;
        _ -> {error, Result}
    end.


%% ============================================================================
%% COMMIT TRACKING - Prevents continuous update loops
%% ============================================================================

read_last_processed_commit(RepoPath) ->
    FilePath = RepoPath ++ "/.last_processed_commit",
    case file:read_file(FilePath) of
        {ok, Bin} -> 
            Commit = string:trim(binary_to_list(Bin)),
            case length(Commit) >= 7 of
                true -> list_to_binary(Commit);
                false -> undefined
            end;
        {error, _} -> undefined
    end.

save_last_processed_commit(RepoPath, Commit) when is_binary(Commit) ->
    FilePath = RepoPath ++ "/.last_processed_commit",
    case file:write_file(FilePath, Commit) of
        ok -> 
            io:format("[GITHUB-WORKER] Saved processed commit: ~s~n", [Commit]),
            ok;
        {error, Reason} ->
            io:format("[GITHUB-WORKER] WARNING: Could not save processed commit: ~p~n", [Reason]),
            {error, Reason}
    end;
save_last_processed_commit(_, _) ->
    ok.

