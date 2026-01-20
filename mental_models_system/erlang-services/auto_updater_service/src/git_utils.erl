%%%-------------------------------------------------------------------
%%% @doc Git Utilities - Clone, fetch, commit operations
%%% @end
%%%-------------------------------------------------------------------
-module(git_utils).

-export([clone_repo/2, get_current_commit/0, get_remote_commit/1]).
-export([fetch_branch/1, reset_hard/1, revert_commit/0]).

%% @doc Clone repository with token auth
clone_repo(Branch, RepoPath) ->
    BranchStr = binary_to_list(Branch),
    case os:getenv("GITHUB_TOKEN") of
        false -> {error, no_token};
        Token ->
            Repo = os:getenv("GITHUB_REPO", "Ripple-Analytics/Ripple_Analytics"),
            Url = "https://" ++ Token ++ "@github.com/" ++ Repo ++ ".git",
            Cmd = "git clone --depth 1 --branch " ++ BranchStr ++ " " ++ Url ++ " " ++ RepoPath ++ " 2>&1",
            case string:find(os:cmd(Cmd), "fatal:") of
                nomatch -> ok;
                _ -> {error, clone_failed}
            end
    end.

%% @doc Get current HEAD commit (7 char hash)
get_current_commit() ->
    Result = string:trim(os:cmd("cd /repo && git rev-parse HEAD 2>/dev/null")),
    case length(Result) >= 7 of
        true -> list_to_binary(string:sub_string(Result, 1, 7));
        false -> undefined
    end.

%% @doc Get remote commit (fetches first)
get_remote_commit(Branch) ->
    fetch_branch(Branch),
    Cmd = "cd /repo && git rev-parse origin/" ++ Branch ++ " 2>/dev/null",
    Result = string:trim(os:cmd(Cmd)),
    case length(Result) >= 7 of
        true -> list_to_binary(string:sub_string(Result, 1, 7));
        false -> undefined
    end.

%% @doc Fetch branch from origin
fetch_branch(Branch) ->
    os:cmd("cd /repo && git fetch origin " ++ Branch ++ " 2>&1").

%% @doc Reset to origin branch
reset_hard(Branch) ->
    os:cmd("cd /repo && git reset --hard origin/" ++ Branch).

%% @doc Revert to previous commit
revert_commit() ->
    os:cmd("cd /repo && git reset --hard HEAD~1 2>&1").
