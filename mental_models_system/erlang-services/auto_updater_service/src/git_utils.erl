%%%-------------------------------------------------------------------
%%% @doc Git Utilities - Clone, fetch, commit operations
%%% @end
%%%-------------------------------------------------------------------
-module(git_utils).

-export([clone_repo/2, get_current_commit/0, get_remote_commit/1]).
-export([fetch_branch/1, reset_hard/1, revert_commit/0]).

-define(REPO_PATH, "/repo").

%% @doc Clone repository with token auth
clone_repo(Branch, RepoPath) ->
    BranchStr = binary_to_list(Branch),
    case os:getenv("GITHUB_TOKEN") of
        false -> {error, no_token};
        Token ->
            Repo = os:getenv("GITHUB_REPO", "Ripple-Analytics/Ripple_Analytics"),
            Url = "https://" ++ Token ++ "@github.com/" ++ Repo ++ ".git",
            Cmd = "git clone --depth 1 --branch " ++ BranchStr ++ " " ++ Url ++ " " ++ RepoPath ++ " 2>&1",
            Result = os:cmd(Cmd),
            case string:find(Result, "fatal:") of
                nomatch -> ok;
                _ -> {error, clone_failed}
            end
    end.

%% @doc Get current HEAD commit (7 char hash)
get_current_commit() ->
    Cmd = "cd " ++ ?REPO_PATH ++ " && git rev-parse HEAD 2>/dev/null",
    Result = string:trim(os:cmd(Cmd)),
    parse_commit_hash(Result).

%% @doc Get remote commit - tries git first, falls back to GitHub API
get_remote_commit(Branch) ->
    %% First try git fetch
    fetch_branch(Branch),
    Cmd = "cd " ++ ?REPO_PATH ++ " && git rev-parse origin/" ++ Branch ++ " 2>/dev/null",
    Result = string:trim(os:cmd(Cmd)),
    case parse_commit_hash(Result) of
        undefined -> 
            %% Fallback to GitHub API
            get_remote_commit_api(Branch);
        Hash -> 
            Hash
    end.

%% @doc Fetch branch from origin
fetch_branch(Branch) ->
    Cmd = "cd " ++ ?REPO_PATH ++ " && git fetch origin " ++ Branch ++ " 2>&1",
    os:cmd(Cmd).

%% @doc Reset to origin branch - fetch first, then use FETCH_HEAD if origin/branch fails
reset_hard(Branch) ->
    io:format("[GIT] Resetting to origin/~s~n", [Branch]),
    
    %% Step 1: Fetch the branch
    FetchCmd = "cd " ++ ?REPO_PATH ++ " && git fetch origin " ++ Branch ++ " 2>&1",
    FetchResult = os:cmd(FetchCmd),
    io:format("[GIT] Fetch result: ~s~n", [truncate(FetchResult, 200)]),
    
    %% Step 2: Try reset to origin/branch first
    ResetCmd1 = "cd " ++ ?REPO_PATH ++ " && git reset --hard origin/" ++ Branch ++ " 2>&1",
    Result1 = os:cmd(ResetCmd1),
    
    case string:find(Result1, "fatal:") of
        nomatch ->
            io:format("[GIT] Reset result: ~s~n", [truncate(Result1, 200)]),
            ok;
        _ ->
            %% Fallback: use FETCH_HEAD
            io:format("[GIT] origin/~s not found, using FETCH_HEAD~n", [Branch]),
            ResetCmd2 = "cd " ++ ?REPO_PATH ++ " && git reset --hard FETCH_HEAD 2>&1",
            Result2 = os:cmd(ResetCmd2),
            io:format("[GIT] Reset result: ~s~n", [truncate(Result2, 200)]),
            case string:find(Result2, "fatal:") of
                nomatch -> ok;
                _ ->
                    %% Last resort: pull with force
                    io:format("[GIT] FETCH_HEAD failed, trying git pull --force~n"),
                    PullCmd = "cd " ++ ?REPO_PATH ++ " && git pull origin " ++ Branch ++ " --force 2>&1",
                    PullResult = os:cmd(PullCmd),
                    io:format("[GIT] Pull result: ~s~n", [truncate(PullResult, 200)]),
                    ok
            end
    end.

%% @doc Revert to previous commit
revert_commit() ->
    io:format("[GIT] Reverting to HEAD~1~n"),
    os:cmd("cd " ++ ?REPO_PATH ++ " && git reset --hard HEAD~1 2>&1").

%% Internal: parse commit hash from git output
parse_commit_hash(Result) when is_list(Result) ->
    %% Check if result looks like a valid hash (40 hex chars)
    Trimmed = string:trim(Result),
    case is_valid_hash(Trimmed) of
        true -> list_to_binary(string:slice(Trimmed, 0, 7));
        false -> undefined
    end;
parse_commit_hash(_) -> undefined.

%% Internal: check if string is a valid git hash
is_valid_hash(Str) when length(Str) >= 7 ->
    lists:all(fun(C) -> 
        (C >= $0 andalso C =< $9) orelse
        (C >= $a andalso C =< $f) orelse
        (C >= $A andalso C =< $F)
    end, Str);
is_valid_hash(_) -> false.

%% Internal: get commit from GitHub API (fallback)
get_remote_commit_api(Branch) ->
    Repo = os:getenv("GITHUB_REPO", "Ripple-Analytics/Ripple_Analytics"),
    Url = "https://api.github.com/repos/" ++ Repo ++ "/commits/" ++ Branch,
    Cmd = "curl -s -H 'Accept: application/vnd.github.v3+json' '" ++ Url ++ "' | grep -m1 '\"sha\"' | cut -d'\"' -f4",
    Result = string:trim(os:cmd(Cmd)),
    parse_commit_hash(Result).

%% Internal: truncate string for logging
truncate(Str, Max) when is_list(Str), length(Str) > Max ->
    string:slice(Str, 0, Max) ++ "...";
truncate(Str, _) when is_list(Str) -> Str;
truncate(_, _) -> "".
