%%%-------------------------------------------------------------------
%%% @doc Updater Utilities - File, Git, and Formatting Helpers
%%% @end
%%%-------------------------------------------------------------------
-module(updater_utils).

-export([read_file/1, write_file/2, delete_file/1]).
-export([fmt/1, format_time/1]).
-export([get_check_interval/0, get_env_binary/2]).
-export([clone_repo/1, get_current_commit/0, get_remote_commit/1]).

-define(DATA_DIR, "/data").

%%====================================================================
%% File Helpers
%%====================================================================

read_file(Path) ->
    case file:read_file(Path) of
        {ok, Bin} -> 
            Trimmed = string:trim(binary_to_list(Bin)),
            case Trimmed of
                "" -> undefined;
                S -> list_to_binary(S)
            end;
        {error, _} -> undefined
    end.

write_file(Path, Content) when is_binary(Content) ->
    file:write_file(Path, Content);
write_file(Path, Content) when is_list(Content) ->
    file:write_file(Path, Content).

delete_file(Path) ->
    file:delete(Path).

%%====================================================================
%% Formatting Helpers
%%====================================================================

fmt(undefined) -> "undefined";
fmt(Bin) when is_binary(Bin) -> binary_to_list(Bin);
fmt(Other) -> io_lib:format("~p", [Other]).

format_time(undefined) -> null;
format_time({MegaSecs, Secs, _}) ->
    Seconds = MegaSecs * 1000000 + Secs,
    list_to_binary(calendar:system_time_to_rfc3339(Seconds, [{unit, second}])).

%%====================================================================
%% Environment Helpers
%%====================================================================

get_check_interval() ->
    case os:getenv("UPDATE_CHECK_INTERVAL") of
        false -> 300000;
        Val -> 
            try list_to_integer(Val) * 1000
            catch _:_ -> 300000
            end
    end.

get_env_binary(Key, Default) ->
    case os:getenv(Key) of
        false -> Default;
        Val -> list_to_binary(Val)
    end.

%%====================================================================
%% Git Helpers
%%====================================================================

clone_repo(Branch) ->
    BranchStr = binary_to_list(Branch),
    case os:getenv("GITHUB_TOKEN") of
        false -> {error, no_token};
        Token ->
            Repo = os:getenv("GITHUB_REPO", "Ripple-Analytics/Ripple_Analytics"),
            Url = "https://" ++ Token ++ "@github.com/" ++ Repo ++ ".git",
            Cmd = "git clone --depth 1 --branch " ++ BranchStr ++ " " ++ Url ++ " /repo 2>&1",
            Result = os:cmd(Cmd),
            case string:find(Result, "fatal:") of
                nomatch -> ok;
                _ -> {error, Result}
            end
    end.

get_current_commit() ->
    Result = string:trim(os:cmd("cd /repo && git rev-parse HEAD 2>/dev/null")),
    case length(Result) >= 7 of
        true -> list_to_binary(string:sub_string(Result, 1, 7));
        false -> undefined
    end.

get_remote_commit(Branch) ->
    io:format("[UPDATER] Fetching from remote...~n"),
    FetchCmd = "cd /repo && git fetch origin " ++ Branch ++ " 2>&1",
    FetchResult = os:cmd(FetchCmd),
    io:format("[UPDATER] Fetch result: ~s~n", [string:sub_string(FetchResult, 1, erlang:min(200, length(FetchResult)))]),
    
    Cmd = "cd /repo && git rev-parse origin/" ++ Branch ++ " 2>/dev/null",
    Result = string:trim(os:cmd(Cmd)),
    io:format("[UPDATER] Remote commit raw: ~s~n", [Result]),
    
    case length(Result) >= 7 of
        true -> 
            Commit = list_to_binary(string:sub_string(Result, 1, 7)),
            io:format("[UPDATER] Remote commit: ~s~n", [Commit]),
            Commit;
        false -> 
            io:format("[UPDATER] Failed to get remote commit~n"),
            undefined
    end.
