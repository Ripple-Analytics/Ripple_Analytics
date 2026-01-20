%%%-------------------------------------------------------------------
%%% @doc updater_worker Helper Module - Part 5
%%% @end
%%%-------------------------------------------------------------------
-module(updater_worker_part5).

-export([switch_traffic/1, read_active_env/0, clone_repo/1, get_current_commit/0, get_remote_commit/1, read_file/1, write_file/2, write_file/2, delete_file/1, fmt/1, fmt/1, fmt/1, format_time/1, format_time/3, get_check_interval/0, get_env_binary/2]).

switch_traffic(NewEnv) ->
    io:format("[UPDATER] Switching traffic to: ~s~n", [NewEnv]),
    
    %% Update active env file for persistence
    write_file(?DATA_DIR ++ "/active_env", NewEnv),
    
    %% Update ALL nginx config files inside the nginx container
    %% Each service has its own config file that needs to be updated
    Services = [
        {"active.conf", "ui-" ++ NewEnv},
        {"active_api.conf", "api-" ++ NewEnv},
        {"active_analysis.conf", "analysis-" ++ NewEnv},
        {"active_harvester.conf", "harvester-" ++ NewEnv},
        {"active_storage.conf", "storage-" ++ NewEnv},
        {"active_updater.conf", "updater-" ++ NewEnv}
    ],
    
    lists:foreach(fun({ConfigFile, Upstream}) ->
        ConfigContent = "# Active environment: " ++ NewEnv ++ "\nlocation / {\n    proxy_pass http://" ++ Upstream ++ ";\n}\n",
        %% Write config directly into nginx container
        Cmd = "docker exec mental-models-proxy sh -c 'echo \"" ++ ConfigContent ++ "\" > /etc/nginx/conf.d/" ++ ConfigFile ++ "'",
        Result = os:cmd(Cmd ++ " 2>&1"),
        io:format("[UPDATER] Updated ~s: ~s~n", [ConfigFile, string:sub_string(Result, 1, erlang:min(100, length(Result)))])
    end, Services),
    
    %% Reload nginx to apply changes
    ReloadResult = os:cmd("docker exec mental-models-proxy nginx -s reload 2>&1"),
    io:format("[UPDATER] Nginx reload: ~s~n", [string:sub_string(ReloadResult, 1, erlang:min(200, length(ReloadResult)))]).

read_active_env() ->
    case read_file(?DATA_DIR ++ "/active_env") of
        undefined -> "blue";
        Env -> binary_to_list(Env)
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
    %% MUST fetch from remote first to get latest commits
    io:format("[UPDATER] Fetching from remote...~n"),
    FetchCmd = "cd /repo && git fetch origin " ++ Branch ++ " 2>&1",
    FetchResult = os:cmd(FetchCmd),
    io:format("[UPDATER] Fetch result: ~s~n", [string:sub_string(FetchResult, 1, erlang:min(200, length(FetchResult)))]),
    
    %% Now get the remote commit hash
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

fmt(undefined) -> "undefined";
fmt(Bin) when is_binary(Bin) -> binary_to_list(Bin);
fmt(Other) -> io_lib:format("~p", [Other]).

format_time(undefined) -> null;
format_time({MegaSecs, Secs, _}) ->
    Seconds = MegaSecs * 1000000 + Secs,
    list_to_binary(calendar:system_time_to_rfc3339(Seconds, [{unit, second}])).

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
%% Phone Home - Report status to external webhook
%%====================================================================

