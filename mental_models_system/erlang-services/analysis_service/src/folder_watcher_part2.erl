%%%-------------------------------------------------------------------
%%% @doc folder_watcher Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(folder_watcher_part2).

-export([handle_call/3, handle_call/4, handle_call/3, handle_call/3, handle_cast/2, handle_info/2, handle_info/2, handle_info/2, terminate/2, code_change/3, get_default_folder/0, ensure_binary/1, ensure_binary/1, start_timer/1, stop_timer/1, stop_timer/1, do_scan/1, find_new_files/2, has_extension/2, analyze_files/2, analyze_single_file/1]).

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

