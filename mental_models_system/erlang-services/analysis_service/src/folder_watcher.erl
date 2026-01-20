%%%-------------------------------------------------------------------
%%% @doc Folder Watcher - Automatic File Monitoring Service
%%% 
%%% Monitors a folder for new text files and automatically analyzes
%%% them for mental models, patterns, and biases.
%%% 
%%% Features:
%%% - Configurable watch interval (default 30 seconds)
%%% - Tracks processed files to avoid re-analysis
%%% - Stores results in DETS for persistence
%%% - Can be started/stopped via API
%%% @end
%%%-------------------------------------------------------------------
-module(folder_watcher).
%% Helper modules: folder_watcher_part2, folder_watcher_part3
-behaviour(gen_server).

-export([start_link/0, start_link/1, stop/0]).
-export([start_watching/0, start_watching/1, stop_watching/0]).
-export([get_status/0, get_results/0, get_results/1, clear_results/0]).
-export([set_config/1, get_config/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_INTERVAL, 30000). % 30 seconds
-define(RESULTS_TABLE, folder_watcher_results).
-define(PROCESSED_TABLE, folder_watcher_processed).

-record(state, {
    watching = false :: boolean(),
    folder_path = <<>> :: binary(),
    interval = ?DEFAULT_INTERVAL :: pos_integer(),
    timer_ref = undefined :: undefined | reference(),
    processed_files = [] :: [binary()],
    results = [] :: list(),
    last_scan = undefined :: undefined | calendar:datetime(),
    files_analyzed = 0 :: non_neg_integer(),
    lollapalooza_count = 0 :: non_neg_integer()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    start_link(#{}).

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

stop() ->
    gen_server:stop(?SERVER).

start_watching() ->
    start_watching(#{}).

start_watching(Config) ->
    gen_server:call(?SERVER, {start_watching, Config}).

stop_watching() ->
    gen_server:call(?SERVER, stop_watching).

get_status() ->
    gen_server:call(?SERVER, get_status).

get_results() ->
    gen_server:call(?SERVER, get_results).

get_results(Limit) ->
    gen_server:call(?SERVER, {get_results, Limit}).

clear_results() ->
    gen_server:call(?SERVER, clear_results).

set_config(Config) ->
    gen_server:call(?SERVER, {set_config, Config}).

get_config() ->
    gen_server:call(?SERVER, get_config).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Config) ->
    FolderPath = maps:get(folder_path, Config, get_default_folder()),
    Interval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
    AutoStart = maps:get(auto_start, Config, false),
    
    State = #state{
        folder_path = FolderPath,
        interval = Interval
    },
    
    FinalState = case AutoStart of
        true -> start_timer(State#state{watching = true});
        false -> State
    end,
    
    io:format("[FolderWatcher] Initialized. Folder: ~s, Interval: ~pms~n", 
              [FolderPath, Interval]),
    {ok, FinalState}.

handle_call({start_watching, Config}, _From, State) ->
    FolderPath = maps:get(folder_path, Config, State#state.folder_path),
    Interval = maps:get(interval, Config, State#state.interval),
    
    NewState = State#state{
        folder_path = ensure_binary(FolderPath),
        interval = Interval,
        watching = true
    },
    
    FinalState = start_timer(NewState),
    io:format("[FolderWatcher] Started watching: ~s~n", [FinalState#state.folder_path]),
    {reply, {ok, #{status => watching, folder => FinalState#state.folder_path}}, FinalState};

handle_call(stop_watching, _From, State) ->
    NewState = stop_timer(State#state{watching = false}),
    io:format("[FolderWatcher] Stopped watching~n"),
    {reply, {ok, #{status => stopped}}, NewState};

handle_call(get_status, _From, State) ->
    Status = #{
        <<"watching">> => State#state.watching,
        <<"folder_path">> => State#state.folder_path,
        <<"interval_ms">> => State#state.interval,
        <<"files_analyzed">> => State#state.files_analyzed,
        <<"lollapalooza_count">> => State#state.lollapalooza_count,
        <<"processed_files">> => length(State#state.processed_files),
        <<"last_scan">> => format_datetime(State#state.last_scan),
        <<"results_count">> => length(State#state.results)
    },
    {reply, {ok, Status}, State};

handle_call(get_results, _From, State) ->
    {reply, {ok, State#state.results}, State};

handle_call({get_results, Limit}, _From, State) ->
    Results = lists:sublist(State#state.results, Limit),
    {reply, {ok, Results}, State};
