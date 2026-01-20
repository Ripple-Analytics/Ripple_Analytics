%%%-------------------------------------------------------------------
%%% @doc Updater Worker
%%% Core update logic with bulletproof fallback chain.
%%% @end
%%%-------------------------------------------------------------------
-module(updater_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([get_status/0, check_updates/0, perform_update/0, get_config/0]).

-record(state, {
    status = idle :: idle | checking | updating | error,
    last_check = undefined,
    last_update = undefined,
    current_commit = undefined,
    remote_commit = undefined,
    branch = <<"master">>,
    update_available = false,
    update_source = undefined,
    error_message = undefined,
    check_interval = 300000,
    timer_ref = undefined
}).

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
    Interval = get_check_interval(),
    self() ! init_repo,
    TimerRef = erlang:send_after(Interval, self(), check_timer),
    {ok, #state{check_interval = Interval, timer_ref = TimerRef}}.

handle_call(get_status, _From, State) ->
    Status = #{
        status => State#state.status,
        last_check => format_time(State#state.last_check),
        last_update => format_time(State#state.last_update),
        current_commit => State#state.current_commit,
        remote_commit => State#state.remote_commit,
        branch => State#state.branch,
        update_available => State#state.update_available,
        update_source => State#state.update_source,
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
        error -> {error, #{message => NewState#state.error_message}};
        _ -> {ok, #{message => <<"Update in progress">>}}
    end,
    {reply, Result, NewState};

handle_call(get_config, _From, State) ->
    Config = #{
        github_repo => get_env_binary("GITHUB_REPO", <<"Ripple-Analytics/Ripple_Analytics">>),
        github_branch => get_env_binary("GITHUB_BRANCH", <<"master">>),
        github_token_configured => os:getenv("GITHUB_TOKEN") =/= false,
        gdrive_configured => os:getenv("GDRIVE_BACKUP_URL") =/= false,
        check_interval_seconds => State#state.check_interval div 1000
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
    FinalState = case NewState#state.update_available of
        true ->
            io:format("[UPDATER] Update available, auto-updating~n"),
            do_perform_update(NewState#state{status = updating});
        false ->
            NewState
    end,
    TimerRef = erlang:send_after(State#state.check_interval, self(), check_timer),
    {noreply, FinalState#state{timer_ref = TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

get_check_interval() ->
    case os:getenv("UPDATE_CHECK_INTERVAL") of
        false -> 300000;
        Val -> list_to_integer(Val) * 1000
    end.

get_env_binary(Key, Default) ->
    case os:getenv(Key) of
        false -> Default;
        Val -> list_to_binary(Val)
    end.

do_init_repo(State) ->
    RepoPath = "/repo",
    case filelib:is_dir(RepoPath ++ "/.git") of
        true ->
            io:format("[UPDATER] Repository already exists~n"),
            Commit = get_current_commit(),
            Branch = get_env_binary("GITHUB_BRANCH", <<"master">>),
            State#state{current_commit = Commit, branch = Branch, status = idle};
        false ->
            io:format("[UPDATER] Cloning repository~n"),
            case try_clone() of
                {ok, Source} ->
                    Commit = get_current_commit(),
                    Branch = get_env_binary("GITHUB_BRANCH", <<"master">>),
                    io:format("[UPDATER] Clone successful via ~p~n", [Source]),
                    State#state{current_commit = Commit, branch = Branch, update_source = Source, status = idle};
                {error, Reason} ->
                    io:format("[UPDATER] Clone failed: ~p~n", [Reason]),
                    State#state{status = error, error_message = <<"Failed to clone repository">>}
            end
    end.

try_clone() ->
    Branch = os:getenv("GITHUB_BRANCH", "master"),
    
    %% Try authenticated HTTPS first (most common)
    case os:getenv("GITHUB_TOKEN") of
        false -> ok;
        Token ->
            Repo = os:getenv("GITHUB_REPO", "Ripple-Analytics/Ripple_Analytics"),
            Url = "https://" ++ Token ++ "@github.com/" ++ Repo ++ ".git",
            Cmd = "git clone --depth 1 --branch " ++ Branch ++ " " ++ Url ++ " /repo 2>&1",
            case run_cmd(Cmd) of
                ok -> 
                    io:format("[UPDATER] Authenticated HTTPS clone successful~n"),
                    {ok, https_auth};
                _ -> ok
            end
    end,
    
    %% Check if clone succeeded
    case filelib:is_dir("/repo/.git") of
        true -> {ok, https_auth};
        false ->
            %% Try public HTTPS
            case os:getenv("GITHUB_REPO_URL") of
                false -> {error, no_sources};
                PublicUrl ->
                    Cmd2 = "git clone --depth 1 --branch " ++ Branch ++ " " ++ PublicUrl ++ " /repo 2>&1",
                    case run_cmd(Cmd2) of
                        ok -> {ok, https_public};
                        _ -> try_gdrive()
                    end
            end
    end.

try_gdrive() ->
    case os:getenv("GDRIVE_BACKUP_URL") of
        false -> {error, no_gdrive};
        Url ->
            Cmd = "curl -L -o /tmp/backup.zip '" ++ Url ++ "' && mkdir -p /repo && unzip -o /tmp/backup.zip -d /repo",
            case run_cmd(Cmd) of
                ok -> {ok, gdrive};
                _ -> {error, gdrive_failed}
            end
    end.

do_check_updates(State) ->
    Now = erlang:timestamp(),
    Cmd = "cd /repo && git fetch origin " ++ binary_to_list(State#state.branch) ++ " --depth 1 2>&1",
    run_cmd(Cmd),
    
    CurrentCommit = get_current_commit(),
    RemoteCommit = get_remote_commit(State#state.branch),
    
    UpdateAvailable = (CurrentCommit =/= undefined) andalso 
                      (RemoteCommit =/= undefined) andalso 
                      (CurrentCommit =/= RemoteCommit),
    
    io:format("[UPDATER] Current: ~p, Remote: ~p, Update: ~p~n", 
              [CurrentCommit, RemoteCommit, UpdateAvailable]),
    
    State#state{
        status = idle,
        last_check = Now,
        current_commit = CurrentCommit,
        remote_commit = RemoteCommit,
        update_available = UpdateAvailable,
        error_message = undefined
    }.

do_perform_update(State) ->
    Now = erlang:timestamp(),
    Branch = binary_to_list(State#state.branch),
    Cmd = "cd /repo && git reset --hard origin/" ++ Branch ++ " 2>&1",
    run_cmd(Cmd),
    
    NewCommit = get_current_commit(),
    io:format("[UPDATER] Updated to: ~p~n", [NewCommit]),
    
    %% Trigger rebuild in background
    spawn(fun() -> rebuild_services() end),
    
    State#state{
        status = idle,
        last_update = Now,
        current_commit = NewCommit,
        update_available = false,
        error_message = undefined
    }.

get_current_commit() ->
    Result = os:cmd("cd /repo && git rev-parse HEAD 2>/dev/null"),
    case string:trim(Result) of
        "" -> undefined;
        Commit when length(Commit) >= 7 -> list_to_binary(string:sub_string(Commit, 1, 7));
        _ -> undefined
    end.

get_remote_commit(Branch) ->
    Cmd = "cd /repo && git rev-parse origin/" ++ binary_to_list(Branch) ++ " 2>/dev/null",
    Result = os:cmd(Cmd),
    case string:trim(Result) of
        "" -> undefined;
        Commit when length(Commit) >= 7 -> list_to_binary(string:sub_string(Commit, 1, 7));
        _ -> undefined
    end.

rebuild_services() ->
    io:format("[UPDATER] Triggering service rebuild~n"),
    os:cmd("cd /repo/mental_models_system/erlang-services && docker-compose build --parallel 2>&1"),
    os:cmd("cd /repo/mental_models_system/erlang-services && docker-compose up -d 2>&1"),
    io:format("[UPDATER] Rebuild complete~n").

run_cmd(Cmd) ->
    Result = os:cmd(Cmd),
    case string:find(Result, "fatal:") of
        nomatch ->
            case string:find(Result, "error:") of
                nomatch -> ok;
                _ -> {error, Result}
            end;
        _ -> {error, Result}
    end.

format_time(undefined) -> null;
format_time({MegaSecs, Secs, _MicroSecs}) ->
    Seconds = MegaSecs * 1000000 + Secs,
    list_to_binary(calendar:system_time_to_rfc3339(Seconds, [{unit, second}])).
