%%%-------------------------------------------------------------------
%%% @doc GitHub Worker
%%% Handles GitHub operations with multiple auth fallbacks.
%%% @end
%%%-------------------------------------------------------------------
-module(github_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([get_status/0, check_updates/0, fetch_latest/0, get_download_path/0]).

-record(state, {
    status = idle,
    last_check = undefined,
    current_commit = undefined,
    remote_commit = undefined,
    branch = <<"master">>,
    update_available = false,
    auth_method = undefined,
    repo_path = "/repo/github",
    error = undefined
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_status() -> gen_server:call(?MODULE, get_status).
check_updates() -> gen_server:call(?MODULE, check_updates, 60000).
fetch_latest() -> gen_server:call(?MODULE, fetch_latest, 120000).
get_download_path() -> gen_server:call(?MODULE, get_download_path).

init([]) ->
    io:format("[GITHUB-WORKER] Starting~n"),
    self() ! init_repo,
    {ok, #state{}}.

handle_call(get_status, _From, State) ->
    Status = #{
        source => <<"github">>,
        status => State#state.status,
        available => State#state.status =/= error,
        last_check => State#state.last_check,
        current_commit => State#state.current_commit,
        remote_commit => State#state.remote_commit,
        update_available => State#state.update_available,
        auth_method => State#state.auth_method,
        error => State#state.error
    },
    {reply, {ok, Status}, State};

handle_call(check_updates, _From, State) ->
    NewState = do_check_updates(State),
    {reply, {ok, #{update_available => NewState#state.update_available}}, NewState};

handle_call(fetch_latest, _From, State) ->
    NewState = do_fetch_latest(State),
    Result = case NewState#state.status of
        idle -> {ok, #{path => list_to_binary(NewState#state.repo_path)}};
        error -> {error, NewState#state.error}
    end,
    {reply, Result, NewState};

handle_call(get_download_path, _From, State) ->
    {reply, {ok, State#state.repo_path}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init_repo, State) ->
    NewState = do_init_repo(State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Internal functions

do_init_repo(State) ->
    RepoPath = State#state.repo_path,
    os:cmd("mkdir -p " ++ RepoPath),
    
    case filelib:is_dir(RepoPath ++ "/.git") of
        true ->
            io:format("[GITHUB-WORKER] Repo exists~n"),
            Commit = get_commit(RepoPath),
            State#state{current_commit = Commit, status = idle};
        false ->
            case try_clone(RepoPath) of
                {ok, Method} ->
                    io:format("[GITHUB-WORKER] Cloned via ~p~n", [Method]),
                    Commit = get_commit(RepoPath),
                    State#state{current_commit = Commit, auth_method = Method, status = idle};
                {error, Reason} ->
                    io:format("[GITHUB-WORKER] Clone failed: ~p~n", [Reason]),
                    State#state{status = error, error = Reason}
            end
    end.

try_clone(RepoPath) ->
    Branch = os:getenv("GITHUB_BRANCH", "master"),
    
    %% Try SSH first
    case os:getenv("GITHUB_SSH_URL") of
        false -> ok;
        SshUrl ->
            case filelib:is_file("/root/.ssh/deploy_key") of
                true ->
                    Cmd = "GIT_SSH_COMMAND='ssh -i /root/.ssh/deploy_key -o StrictHostKeyChecking=no' git clone --depth 1 --branch " ++ Branch ++ " " ++ SshUrl ++ " " ++ RepoPath ++ " 2>&1",
                    case run_git(Cmd) of
                        ok -> {ok, ssh};
                        _ -> ok
                    end;
                false -> ok
            end
    end,
    
    case filelib:is_dir(RepoPath ++ "/.git") of
        true -> {ok, ssh};
        false -> try_https_auth(RepoPath, Branch)
    end.

try_https_auth(RepoPath, Branch) ->
    case os:getenv("GITHUB_TOKEN") of
        false -> try_https_public(RepoPath, Branch);
        Token ->
            Repo = os:getenv("GITHUB_REPO", "Ripple-Analytics/Ripple_Analytics"),
            Url = "https://" ++ Token ++ "@github.com/" ++ Repo ++ ".git",
            Cmd = "git clone --depth 1 --branch " ++ Branch ++ " " ++ Url ++ " " ++ RepoPath ++ " 2>&1",
            case run_git(Cmd) of
                ok -> {ok, https_auth};
                _ -> try_https_public(RepoPath, Branch)
            end
    end.

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
    
    UpdateAvailable = (Current =/= undefined) andalso (Remote =/= undefined) andalso (Current =/= Remote),
    
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
    State#state{current_commit = NewCommit, update_available = false, status = idle}.

get_commit(RepoPath) ->
    Result = os:cmd("cd " ++ RepoPath ++ " && git rev-parse HEAD 2>/dev/null"),
    case string:trim(Result) of
        "" -> undefined;
        C when length(C) >= 7 -> list_to_binary(string:sub_string(C, 1, 7));
        _ -> undefined
    end.

get_remote_commit(RepoPath, Branch) ->
    Result = os:cmd("cd " ++ RepoPath ++ " && git rev-parse origin/" ++ Branch ++ " 2>/dev/null"),
    case string:trim(Result) of
        "" -> undefined;
        C when length(C) >= 7 -> list_to_binary(string:sub_string(C, 1, 7));
        _ -> undefined
    end.

run_git(Cmd) ->
    Result = os:cmd(Cmd),
    case string:find(Result, "fatal:") of
        nomatch -> ok;
        _ -> {error, Result}
    end.
