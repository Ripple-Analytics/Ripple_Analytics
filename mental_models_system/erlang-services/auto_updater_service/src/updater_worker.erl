%%%-------------------------------------------------------------------
%%% @doc Updater Worker
%%% Core update logic with bulletproof fallback chain.
%%% Handles automatic background checks and manual update triggers.
%%% @end
%%%-------------------------------------------------------------------
-module(updater_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([get_status/0, check_updates/0, perform_update/0, get_config/0]).

-record(state, {
    status = idle,
    last_check = undefined,
    last_update = undefined,
    current_commit = undefined,
    remote_commit = undefined,
    update_available = false,
    update_source = undefined,
    sources_tried = [],
    error_message = undefined,
    check_interval = 300000,  % 5 minutes in ms
    timer_ref = undefined
}).

%% Configuration from environment
-define(GITHUB_SSH_URL, os:getenv("GITHUB_SSH_URL", "git@github.com:Ripple-Analytics/Ripple_Analytics.git")).
-define(GITHUB_HTTPS_URL, os:getenv("GITHUB_REPO_URL", "https://github.com/Ripple-Analytics/Ripple_Analytics.git")).
-define(GITHUB_TOKEN, os:getenv("GITHUB_TOKEN", "")).
-define(GITHUB_REPO, os:getenv("GITHUB_REPO", "Ripple-Analytics/Ripple_Analytics")).
-define(GITHUB_BRANCH, os:getenv("GITHUB_BRANCH", "master")).
-define(GDRIVE_BACKUP_URL, os:getenv("GDRIVE_BACKUP_URL", "")).
-define(REPO_PATH, "/repo").
-define(SERVICES_PATH, "/repo/mental_models_system/erlang-services").

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
    gen_server:call(?MODULE, perform_update, 120000).

get_config() ->
    gen_server:call(?MODULE, get_config).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[UPDATER] Worker starting~n"),
    
    %% Initialize repository if needed
    self() ! init_repo,
    
    %% Start periodic check timer
    Interval = list_to_integer(os:getenv("UPDATE_CHECK_INTERVAL", "300")) * 1000,
    TimerRef = erlang:send_after(Interval, self(), check_timer),
    
    {ok, #state{check_interval = Interval, timer_ref = TimerRef}}.

handle_call(get_status, _From, State) ->
    Status = #{
        status => State#state.status,
        last_check => format_time(State#state.last_check),
        last_update => format_time(State#state.last_update),
        current_commit => State#state.current_commit,
        remote_commit => State#state.remote_commit,
        update_available => State#state.update_available,
        update_source => State#state.update_source,
        sources_tried => State#state.sources_tried,
        error_message => State#state.error_message,
        check_interval_seconds => State#state.check_interval div 1000
    },
    {reply, {ok, Status}, State};

handle_call(check_updates, _From, State) ->
    io:format("[UPDATER] Manual check triggered~n"),
    NewState = do_check_updates(State#state{status = checking}),
    {reply, {ok, #{update_available => NewState#state.update_available}}, NewState};

handle_call(perform_update, _From, State) ->
    io:format("[UPDATER] Manual update triggered~n"),
    NewState = do_perform_update(State#state{status = updating}),
    Result = case NewState#state.status of
        idle -> {ok, #{message => <<"Update successful">>, source => NewState#state.update_source}};
        error -> {error, #{message => NewState#state.error_message}}
    end,
    {reply, Result, NewState};

handle_call(get_config, _From, State) ->
    Config = #{
        github_ssh_url => list_to_binary(?GITHUB_SSH_URL),
        github_https_url => list_to_binary(?GITHUB_HTTPS_URL),
        github_token_configured => ?GITHUB_TOKEN =/= "",
        github_repo => list_to_binary(?GITHUB_REPO),
        github_branch => list_to_binary(?GITHUB_BRANCH),
        gdrive_configured => ?GDRIVE_BACKUP_URL =/= "",
        check_interval_seconds => State#state.check_interval div 1000,
        repo_path => list_to_binary(?REPO_PATH)
    },
    {reply, {ok, Config}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init_repo, State) ->
    io:format("[UPDATER] Initializing repository~n"),
    NewState = do_init_repo(State),
    {noreply, NewState};

handle_info(check_timer, State) ->
    io:format("[UPDATER] Periodic check triggered~n"),
    NewState = do_check_updates(State#state{status = checking}),
    
    %% If update available, perform it automatically
    FinalState = case NewState#state.update_available of
        true ->
            io:format("[UPDATER] Update available, auto-updating~n"),
            do_perform_update(NewState#state{status = updating});
        false ->
            NewState
    end,
    
    %% Schedule next check
    TimerRef = erlang:send_after(State#state.check_interval, self(), check_timer),
    {noreply, FinalState#state{timer_ref = TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

do_init_repo(State) ->
    case filelib:is_dir(?REPO_PATH ++ "/.git") of
        true ->
            io:format("[UPDATER] Repository already initialized~n"),
            CurrentCommit = get_current_commit(),
            State#state{current_commit = CurrentCommit, status = idle};
        false ->
            io:format("[UPDATER] Cloning repository~n"),
            case try_clone_sources() of
                {ok, Source} ->
                    CurrentCommit = get_current_commit(),
                    io:format("[UPDATER] Repository cloned via ~s~n", [Source]),
                    State#state{current_commit = CurrentCommit, update_source = Source, status = idle};
                {error, SourcesTried} ->
                    io:format("[UPDATER] Failed to clone from any source~n"),
                    State#state{status = error, error_message = <<"Failed to initialize repository">>, sources_tried = SourcesTried}
            end
    end.

do_check_updates(State) ->
    Now = erlang:timestamp(),
    case try_fetch() of
        {ok, RemoteCommit} ->
            CurrentCommit = get_current_commit(),
            UpdateAvailable = CurrentCommit =/= RemoteCommit,
            io:format("[UPDATER] Current: ~s, Remote: ~s, Update: ~p~n", 
                      [CurrentCommit, RemoteCommit, UpdateAvailable]),
            State#state{
                status = idle,
                last_check = Now,
                current_commit = CurrentCommit,
                remote_commit = RemoteCommit,
                update_available = UpdateAvailable,
                error_message = undefined
            };
        {error, Reason} ->
            io:format("[UPDATER] Check failed: ~p~n", [Reason]),
            State#state{
                status = idle,
                last_check = Now,
                error_message = list_to_binary(io_lib:format("~p", [Reason]))
            }
    end.

do_perform_update(State) ->
    Now = erlang:timestamp(),
    case try_pull_sources() of
        {ok, Source} ->
            io:format("[UPDATER] Update successful via ~s~n", [Source]),
            CurrentCommit = get_current_commit(),
            %% Trigger service rebuild
            rebuild_services(),
            State#state{
                status = idle,
                last_update = Now,
                current_commit = CurrentCommit,
                update_available = false,
                update_source = Source,
                error_message = undefined
            };
        {error, SourcesTried} ->
            io:format("[UPDATER] Update failed from all sources~n"),
            State#state{
                status = error,
                sources_tried = SourcesTried,
                error_message = <<"All update sources failed">>
            }
    end.

try_clone_sources() ->
    Sources = [
        {ssh, fun clone_via_ssh/0},
        {https_auth, fun clone_via_https_auth/0},
        {https_public, fun clone_via_https_public/0},
        {gdrive, fun clone_via_gdrive/0}
    ],
    try_sources(Sources, []).

try_pull_sources() ->
    Sources = [
        {ssh, fun pull_via_ssh/0},
        {https_auth, fun pull_via_https_auth/0},
        {https_public, fun pull_via_https_public/0},
        {gdrive, fun clone_via_gdrive/0}
    ],
    try_sources(Sources, []).

try_sources([], Tried) ->
    {error, lists:reverse(Tried)};
try_sources([{Name, Fun} | Rest], Tried) ->
    io:format("[UPDATER] Trying source: ~s~n", [Name]),
    case catch Fun() of
        ok ->
            {ok, Name};
        {ok, _} ->
            {ok, Name};
        Error ->
            io:format("[UPDATER] Source ~s failed: ~p~n", [Name, Error]),
            try_sources(Rest, [Name | Tried])
    end.

clone_via_ssh() ->
    Cmd = io_lib:format("git clone --depth 1 --branch ~s ~s ~s 2>&1", 
                        [?GITHUB_BRANCH, ?GITHUB_SSH_URL, ?REPO_PATH]),
    run_cmd(Cmd).

clone_via_https_auth() ->
    case ?GITHUB_TOKEN of
        "" -> {error, no_token};
        Token ->
            Url = io_lib:format("https://~s@github.com/~s.git", [Token, ?GITHUB_REPO]),
            Cmd = io_lib:format("git clone --depth 1 --branch ~s ~s ~s 2>&1",
                               [?GITHUB_BRANCH, Url, ?REPO_PATH]),
            run_cmd(Cmd)
    end.

clone_via_https_public() ->
    Cmd = io_lib:format("git clone --depth 1 --branch ~s ~s ~s 2>&1",
                        [?GITHUB_BRANCH, ?GITHUB_HTTPS_URL, ?REPO_PATH]),
    run_cmd(Cmd).

clone_via_gdrive() ->
    case ?GDRIVE_BACKUP_URL of
        "" -> {error, no_gdrive_url};
        Url ->
            %% Download and extract from Google Drive
            Cmd = io_lib:format(
                "curl -L -o /tmp/backup.zip '~s' && "
                "rm -rf ~s && mkdir -p ~s && "
                "unzip -o /tmp/backup.zip -d ~s && "
                "rm /tmp/backup.zip",
                [Url, ?REPO_PATH, ?REPO_PATH, ?REPO_PATH]),
            run_cmd(Cmd)
    end.

pull_via_ssh() ->
    Cmd = io_lib:format(
        "cd ~s && git remote set-url origin ~s && "
        "git fetch origin ~s --depth 1 && "
        "git reset --hard origin/~s 2>&1",
        [?REPO_PATH, ?GITHUB_SSH_URL, ?GITHUB_BRANCH, ?GITHUB_BRANCH]),
    run_cmd(Cmd).

pull_via_https_auth() ->
    case ?GITHUB_TOKEN of
        "" -> {error, no_token};
        Token ->
            Url = io_lib:format("https://~s@github.com/~s.git", [Token, ?GITHUB_REPO]),
            Cmd = io_lib:format(
                "cd ~s && git remote set-url origin ~s && "
                "git fetch origin ~s --depth 1 && "
                "git reset --hard origin/~s 2>&1",
                [?REPO_PATH, Url, ?GITHUB_BRANCH, ?GITHUB_BRANCH]),
            run_cmd(Cmd)
    end.

pull_via_https_public() ->
    Cmd = io_lib:format(
        "cd ~s && git remote set-url origin ~s && "
        "git fetch origin ~s --depth 1 && "
        "git reset --hard origin/~s 2>&1",
        [?REPO_PATH, ?GITHUB_HTTPS_URL, ?GITHUB_BRANCH, ?GITHUB_BRANCH]),
    run_cmd(Cmd).

try_fetch() ->
    Cmd = io_lib:format("cd ~s && git fetch origin ~s --depth 1 2>&1 && git rev-parse origin/~s",
                        [?REPO_PATH, ?GITHUB_BRANCH, ?GITHUB_BRANCH]),
    case os:cmd(Cmd) of
        Result ->
            Lines = string:tokens(Result, "\n"),
            case lists:last(Lines) of
                Commit when length(Commit) == 40 ->
                    {ok, list_to_binary(Commit)};
                _ ->
                    {error, fetch_failed}
            end
    end.

get_current_commit() ->
    Cmd = io_lib:format("cd ~s && git rev-parse HEAD 2>/dev/null", [?REPO_PATH]),
    case string:trim(os:cmd(Cmd)) of
        Commit when length(Commit) == 40 ->
            list_to_binary(Commit);
        _ ->
            undefined
    end.

rebuild_services() ->
    io:format("[UPDATER] Triggering service rebuild~n"),
    Cmd = io_lib:format(
        "cd ~s && docker-compose build --no-cache api-gateway analysis-service "
        "harvester-service storage-service chaos-engineering desktop-ui 2>&1 && "
        "docker-compose up -d api-gateway analysis-service harvester-service "
        "storage-service chaos-engineering desktop-ui 2>&1",
        [?SERVICES_PATH]),
    spawn(fun() -> os:cmd(Cmd) end),
    ok.

run_cmd(Cmd) ->
    Result = os:cmd(lists:flatten(Cmd)),
    case string:find(Result, "fatal:") of
        nomatch ->
            case string:find(Result, "error:") of
                nomatch -> ok;
                _ -> {error, Result}
            end;
        _ ->
            {error, Result}
    end.

format_time(undefined) -> null;
format_time({MegaSecs, Secs, _MicroSecs}) ->
    Seconds = MegaSecs * 1000000 + Secs,
    list_to_binary(calendar:system_time_to_rfc3339(Seconds, [{unit, second}])).
