-module(git_integration).
-behaviour(gen_server).

-export([start_link/0, commit_improvement/2, create_branch/1, get_status/0]).
-export([get_current_branch/0, get_recent_commits/1, push_changes/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    repo_path :: string(),
    current_branch :: string(),
    auto_commit :: boolean(),
    auto_push :: boolean()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

commit_improvement(FilePath, Description) ->
    gen_server:call(?MODULE, {commit_improvement, FilePath, Description}, 30000).

create_branch(BranchName) ->
    gen_server:call(?MODULE, {create_branch, BranchName}, 10000).

get_status() ->
    gen_server:call(?MODULE, get_status, 10000).

get_current_branch() ->
    gen_server:call(?MODULE, get_current_branch, 5000).

get_recent_commits(Count) ->
    gen_server:call(?MODULE, {get_recent_commits, Count}, 10000).

push_changes() ->
    gen_server:call(?MODULE, push_changes, 30000).

init([]) ->
    RepoPath = "/app/services",
    AutoCommit = application:get_env(ai_code_improver, auto_commit, true),
    AutoPush = application:get_env(ai_code_improver, auto_push, false),
    {ok, #state{
        repo_path = RepoPath,
        current_branch = "release",
        auto_commit = AutoCommit,
        auto_push = AutoPush
    }}.

handle_call({commit_improvement, FilePath, Description}, _From, State) ->
    Result = do_commit_improvement(State#state.repo_path, FilePath, Description),
    case Result of
        {ok, CommitHash} when State#state.auto_push ->
            do_push(State#state.repo_path),
            {reply, {ok, CommitHash}, State};
        _ ->
            {reply, Result, State}
    end;

handle_call({create_branch, BranchName}, _From, State) ->
    Cmd = io_lib:format("cd ~s && git checkout -b ~s", [State#state.repo_path, BranchName]),
    Result = os:cmd(lists:flatten(Cmd)),
    {reply, {ok, Result}, State#state{current_branch = BranchName}};

handle_call(get_status, _From, State) ->
    Cmd = io_lib:format("cd ~s && git status --porcelain", [State#state.repo_path]),
    Result = os:cmd(lists:flatten(Cmd)),
    Status = #{
        <<"repo_path">> => list_to_binary(State#state.repo_path),
        <<"current_branch">> => list_to_binary(State#state.current_branch),
        <<"auto_commit">> => State#state.auto_commit,
        <<"auto_push">> => State#state.auto_push,
        <<"changes">> => list_to_binary(Result)
    },
    {reply, {ok, Status}, State};

handle_call(get_current_branch, _From, State) ->
    Cmd = io_lib:format("cd ~s && git branch --show-current", [State#state.repo_path]),
    Result = string:trim(os:cmd(lists:flatten(Cmd))),
    {reply, {ok, list_to_binary(Result)}, State#state{current_branch = Result}};

handle_call({get_recent_commits, Count}, _From, State) ->
    Cmd = io_lib:format("cd ~s && git log --oneline -~p", [State#state.repo_path, Count]),
    Result = os:cmd(lists:flatten(Cmd)),
    Lines = string:split(Result, "\n", all),
    Commits = lists:filtermap(fun(Line) ->
        case string:trim(Line) of
            "" -> false;
            L -> {true, list_to_binary(L)}
        end
    end, Lines),
    {reply, {ok, Commits}, State};

handle_call(push_changes, _From, State) ->
    Result = do_push(State#state.repo_path),
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

do_commit_improvement(RepoPath, FilePath, Description) ->
    AddCmd = io_lib:format("cd ~s && git add ~s", [RepoPath, FilePath]),
    os:cmd(lists:flatten(AddCmd)),
    
    CommitMsg = io_lib:format("[AI Improver] ~s", [Description]),
    CommitCmd = io_lib:format("cd ~s && git commit -m \"~s\"", [RepoPath, CommitMsg]),
    CommitResult = os:cmd(lists:flatten(CommitCmd)),
    
    HashCmd = io_lib:format("cd ~s && git rev-parse --short HEAD", [RepoPath]),
    Hash = string:trim(os:cmd(lists:flatten(HashCmd))),
    
    case string:find(CommitResult, "nothing to commit") of
        nomatch -> {ok, list_to_binary(Hash)};
        _ -> {error, nothing_to_commit}
    end.

do_push(RepoPath) ->
    Cmd = io_lib:format("cd ~s && git push origin HEAD", [RepoPath]),
    Result = os:cmd(lists:flatten(Cmd)),
    case string:find(Result, "error") of
        nomatch -> {ok, pushed};
        _ -> {error, list_to_binary(Result)}
    end.
