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

handle_call(clear_results, _From, State) ->
    NewState = State#state{
        results = [],
        processed_files = [],
        files_analyzed = 0,
        lollapalooza_count = 0
    },
    {reply, {ok, #{status => cleared}}, NewState};

handle_call({set_config, Config}, _From, State) ->
    FolderPath = maps:get(folder_path, Config, State#state.folder_path),
    Interval = maps:get(interval, Config, State#state.interval),
    
    NewState = State#state{
        folder_path = ensure_binary(FolderPath),
        interval = Interval
    },
    
    FinalState = case State#state.watching of
        true -> start_timer(stop_timer(NewState));
        false -> NewState
    end,
    
    {reply, {ok, #{folder_path => FinalState#state.folder_path, interval => FinalState#state.interval}}, FinalState};

handle_call(get_config, _From, State) ->
    Config = #{
        <<"folder_path">> => State#state.folder_path,
        <<"interval_ms">> => State#state.interval,
        <<"watching">> => State#state.watching
    },
    {reply, {ok, Config}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(scan_folder, State) when State#state.watching ->
    NewState = do_scan(State),
    FinalState = start_timer(NewState),
    {noreply, FinalState};

handle_info(scan_folder, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    stop_timer(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

get_default_folder() ->
    case os:getenv("HOST_PATH") of
        false -> <<"/data">>;
        Path -> list_to_binary(Path)
    end.

ensure_binary(Bin) when is_binary(Bin) -> Bin;
ensure_binary(List) when is_list(List) -> list_to_binary(List).

start_timer(State) ->
    NewState = stop_timer(State),
    Ref = erlang:send_after(State#state.interval, self(), scan_folder),
    NewState#state{timer_ref = Ref}.

stop_timer(#state{timer_ref = undefined} = State) ->
    State;
stop_timer(#state{timer_ref = Ref} = State) ->
    erlang:cancel_timer(Ref),
    State#state{timer_ref = undefined}.

do_scan(State) ->
    FolderPath = binary_to_list(State#state.folder_path),
    
    case filelib:is_dir(FolderPath) of
        true ->
            Files = find_new_files(FolderPath, State#state.processed_files),
            case Files of
                [] ->
                    State#state{last_scan = calendar:local_time()};
                _ ->
                    io:format("[FolderWatcher] Found ~p new files to analyze~n", [length(Files)]),
                    analyze_files(Files, State)
            end;
        false ->
            io:format("[FolderWatcher] Folder not found: ~s~n", [FolderPath]),
            State#state{last_scan = calendar:local_time()}
    end.

find_new_files(FolderPath, ProcessedFiles) ->
    Extensions = folder_scraper:get_supported_extensions(),
    AllFiles = case file:list_dir(FolderPath) of
        {ok, Entries} ->
            [filename:join(FolderPath, E) || E <- Entries,
             filelib:is_regular(filename:join(FolderPath, E)),
             has_extension(E, Extensions)];
        {error, _} ->
            []
    end,
    
    [F || F <- AllFiles, not lists:member(list_to_binary(F), ProcessedFiles)].

has_extension(Filename, Extensions) ->
    Ext = string:lowercase(filename:extension(Filename)),
    lists:member(Ext, Extensions).

analyze_files(Files, State) ->
    Results = [analyze_single_file(F) || F <- Files],
    
    NewProcessed = [list_to_binary(F) || F <- Files] ++ State#state.processed_files,
    NewResults = Results ++ State#state.results,
    
    LollapaloozaCount = length([R || R <- Results, 
                                     maps:get(<<"lollapalooza_detected">>, R, false) =:= true]),
    
    State#state{
        processed_files = NewProcessed,
        results = NewResults,
        files_analyzed = State#state.files_analyzed + length(Files),
        lollapalooza_count = State#state.lollapalooza_count + LollapaloozaCount,
        last_scan = calendar:local_time()
    }.

analyze_single_file(FilePath) ->
    io:format("[FolderWatcher] Analyzing: ~s~n", [FilePath]),
    Result = folder_scraper:analyze_file(FilePath, #{analysis_type => full}),
    
    %% Send notification for new file analysis
    FileName = list_to_binary(filename:basename(FilePath)),
    notify_file_analyzed(FileName, Result),
    
    Result#{
        <<"analyzed_at">> => format_datetime(calendar:local_time()),
        <<"auto_analyzed">> => true
    }.

notify_file_analyzed(FileName, Result) ->
    LollapaloozaDetected = maps:get(<<"lollapalooza_detected">>, Result, false),
    Models = maps:get(<<"models">>, Result, []),
    TopModels = lists:sublist([maps:get(<<"name">>, M, <<"Unknown">>) || M <- Models], 3),
    
    case LollapaloozaDetected of
        true ->
            notification_service:notify(lollapalooza, #{
                <<"file">> => FileName,
                <<"message">> => <<"Lollapalooza effect detected!">>,
                <<"models">> => TopModels,
                <<"severity">> => <<"high">>
            });
        false when length(Models) > 0 ->
            notification_service:notify(analysis_complete, #{
                <<"file">> => FileName,
                <<"message">> => <<"Analysis complete">>,
                <<"models_found">> => length(Models),
                <<"top_models">> => TopModels
            });
        _ ->
            notification_service:notify(analysis_complete, #{
                <<"file">> => FileName,
                <<"message">> => <<"Analysis complete - no models detected">>
            })
    end.

format_datetime(undefined) -> <<"never">>;
format_datetime({{Y, M, D}, {H, Mi, S}}) ->
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", 
                                  [Y, M, D, H, Mi, S])).
