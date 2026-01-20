%%%-------------------------------------------------------------------
%%% @doc Cache Worker
%%% Manages local cache of code versions for rollback capability.
%%% @end
%%%-------------------------------------------------------------------
-module(cache_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([get_status/0, get_versions/0, save_version/2, get_latest/0, rollback/1]).

-record(state, {
    cache_dir = "/cache",
    versions = [],
    current_version = undefined,
    max_versions = 5
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_status() -> gen_server:call(?MODULE, get_status).
get_versions() -> gen_server:call(?MODULE, get_versions).
save_version(SourcePath, Commit) -> gen_server:call(?MODULE, {save_version, SourcePath, Commit}, 120000).
get_latest() -> gen_server:call(?MODULE, get_latest).
rollback(Version) -> gen_server:call(?MODULE, {rollback, Version}).

init([]) ->
    io:format("[CACHE-WORKER] Starting~n"),
    CacheDir = "/cache",
    os:cmd("mkdir -p " ++ CacheDir),
    Versions = scan_versions(CacheDir),
    {ok, #state{cache_dir = CacheDir, versions = Versions}}.

handle_call(get_status, _From, State) ->
    Status = #{
        source => <<"local_cache">>,
        status => idle,
        available => length(State#state.versions) > 0,
        versions_count => length(State#state.versions),
        current_version => State#state.current_version,
        cache_dir => list_to_binary(State#state.cache_dir)
    },
    {reply, {ok, Status}, State};

handle_call(get_versions, _From, State) ->
    {reply, {ok, State#state.versions}, State};

handle_call({save_version, SourcePath, Commit}, _From, State) ->
    NewState = do_save_version(SourcePath, Commit, State),
    {reply, {ok, #{saved => true, version => Commit}}, NewState};

handle_call(get_latest, _From, State) ->
    case State#state.versions of
        [] -> {reply, {error, <<"No cached versions">>}, State};
        [Latest|_] -> 
            Path = State#state.cache_dir ++ "/" ++ binary_to_list(Latest),
            {reply, {ok, #{path => list_to_binary(Path), version => Latest}}, State}
    end;

handle_call({rollback, Version}, _From, State) ->
    Path = State#state.cache_dir ++ "/" ++ binary_to_list(Version),
    case filelib:is_dir(Path) of
        true -> 
            {reply, {ok, #{path => list_to_binary(Path), version => Version}}, 
             State#state{current_version = Version}};
        false -> 
            {reply, {error, <<"Version not found">>}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Internal functions

scan_versions(CacheDir) ->
    case file:list_dir(CacheDir) of
        {ok, Files} ->
            Dirs = [list_to_binary(F) || F <- Files, 
                    filelib:is_dir(CacheDir ++ "/" ++ F)],
            lists:reverse(lists:sort(Dirs));
        _ -> []
    end.

do_save_version(SourcePath, Commit, State) ->
    Timestamp = integer_to_list(erlang:system_time(second)),
    VersionName = Timestamp ++ "_" ++ binary_to_list(Commit),
    DestPath = State#state.cache_dir ++ "/" ++ VersionName,
    
    io:format("[CACHE-WORKER] Saving version ~s~n", [VersionName]),
    
    %% Copy source to cache
    Cmd = "cp -r " ++ SourcePath ++ " " ++ DestPath,
    os:cmd(Cmd),
    
    %% Update versions list
    NewVersions = [list_to_binary(VersionName) | State#state.versions],
    
    %% Prune old versions if needed
    FinalVersions = prune_versions(NewVersions, State#state.max_versions, State#state.cache_dir),
    
    State#state{versions = FinalVersions, current_version = list_to_binary(VersionName)}.

prune_versions(Versions, Max, CacheDir) when length(Versions) > Max ->
    {Keep, Remove} = lists:split(Max, Versions),
    lists:foreach(fun(V) ->
        Path = CacheDir ++ "/" ++ binary_to_list(V),
        os:cmd("rm -rf " ++ Path)
    end, Remove),
    Keep;
prune_versions(Versions, _Max, _CacheDir) ->
    Versions.
