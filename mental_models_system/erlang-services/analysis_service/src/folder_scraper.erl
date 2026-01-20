%%%-------------------------------------------------------------------
%%% @doc Folder Scraper - Batch File Analysis
%%% 
%%% Automatically scans a folder for text files and analyzes them
%%% for mental models, patterns, and biases.
%%% @end
%%%-------------------------------------------------------------------
-module(folder_scraper).

-include_lib("kernel/include/file.hrl").

-export([scan_folder/1, scan_folder/2, analyze_file/1, analyze_file/2,
         get_supported_extensions/0, scan_and_analyze/1, scan_and_analyze/2]).

-define(DEFAULT_EXTENSIONS, [".txt", ".md", ".text", ".notes", ".doc"]).
-define(MAX_FILE_SIZE, 1048576). % 1MB max file size

%%====================================================================
%% API
%%====================================================================

%% @doc Get list of supported file extensions
get_supported_extensions() ->
    ?DEFAULT_EXTENSIONS.

%% @doc Scan a folder and return list of analyzable files
scan_folder(FolderPath) ->
    scan_folder(FolderPath, #{}).

scan_folder(FolderPath, Options) when is_binary(FolderPath) ->
    scan_folder(binary_to_list(FolderPath), Options);
scan_folder(FolderPath, Options) ->
    Extensions = maps:get(extensions, Options, ?DEFAULT_EXTENSIONS),
    Recursive = maps:get(recursive, Options, false),
    
    case filelib:is_dir(FolderPath) of
        true ->
            Files = find_files(FolderPath, Extensions, Recursive),
            #{
                <<"success">> => true,
                <<"folder">> => list_to_binary(FolderPath),
                <<"files">> => [file_info(F) || F <- Files],
                <<"file_count">> => length(Files),
                <<"extensions">> => [list_to_binary(E) || E <- Extensions]
            };
        false ->
            #{
                <<"success">> => false,
                <<"error">> => <<"Folder not found or not accessible">>,
                <<"folder">> => list_to_binary(FolderPath)
            }
    end.

%% @doc Analyze a single file
analyze_file(FilePath) ->
    analyze_file(FilePath, #{}).

analyze_file(FilePath, Options) when is_binary(FilePath) ->
    analyze_file(binary_to_list(FilePath), Options);
analyze_file(FilePath, Options) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            FileSize = byte_size(Content),
            case FileSize > ?MAX_FILE_SIZE of
                true ->
                    #{
                        <<"success">> => false,
                        <<"error">> => <<"File too large (max 1MB)">>,
                        <<"file">> => list_to_binary(FilePath),
                        <<"size">> => FileSize
                    };
                false ->
                    AnalysisType = maps:get(analysis_type, Options, full),
                    Result = run_analysis(Content, AnalysisType),
                    Result#{
                        <<"file">> => list_to_binary(FilePath),
                        <<"file_size">> => FileSize,
                        <<"file_name">> => list_to_binary(filename:basename(FilePath))
                    }
            end;
        {error, Reason} ->
            #{
                <<"success">> => false,
                <<"error">> => list_to_binary(io_lib:format("~p", [Reason])),
                <<"file">> => list_to_binary(FilePath)
            }
    end.

%% @doc Scan folder and analyze all files
scan_and_analyze(FolderPath) ->
    scan_and_analyze(FolderPath, #{}).

scan_and_analyze(FolderPath, Options) when is_binary(FolderPath) ->
    scan_and_analyze(binary_to_list(FolderPath), Options);
scan_and_analyze(FolderPath, Options) ->
    Extensions = maps:get(extensions, Options, ?DEFAULT_EXTENSIONS),
    Recursive = maps:get(recursive, Options, false),
    AnalysisType = maps:get(analysis_type, Options, full),
    
    case filelib:is_dir(FolderPath) of
        true ->
            Files = find_files(FolderPath, Extensions, Recursive),
            Results = [analyze_file(F, #{analysis_type => AnalysisType}) || F <- Files],
            
            %% Calculate summary statistics
            SuccessCount = length([R || R <- Results, maps:get(<<"success">>, R, false) =:= true]),
            FailCount = length(Results) - SuccessCount,
            
            %% Aggregate all detected models across files
            AllModels = lists:flatten([maps:get(<<"models">>, R, []) || R <- Results]),
            ModelCounts = count_models(AllModels),
            
            %% Find files with Lollapalooza effects
            LollapaloozaFiles = [maps:get(<<"file_name">>, R, <<"unknown">>) || 
                                 R <- Results, 
                                 maps:get(<<"lollapalooza_detected">>, R, false) =:= true],
            
            #{
                <<"success">> => true,
                <<"folder">> => list_to_binary(FolderPath),
                <<"total_files">> => length(Files),
                <<"analyzed">> => SuccessCount,
                <<"failed">> => FailCount,
                <<"results">> => Results,
                <<"summary">> => #{
                    <<"top_models">> => top_n_models(ModelCounts, 10),
                    <<"lollapalooza_files">> => LollapaloozaFiles,
                    <<"lollapalooza_count">> => length(LollapaloozaFiles)
                }
            };
        false ->
            #{
                <<"success">> => false,
                <<"error">> => <<"Folder not found or not accessible">>,
                <<"folder">> => list_to_binary(FolderPath)
            }
    end.

%%====================================================================
%% Internal functions
%%====================================================================

find_files(FolderPath, Extensions, Recursive) ->
    case Recursive of
        true ->
            filelib:fold_files(FolderPath, ".*", true, 
                fun(F, Acc) ->
                    case has_extension(F, Extensions) of
                        true -> [F | Acc];
                        false -> Acc
                    end
                end, []);
        false ->
            case file:list_dir(FolderPath) of
                {ok, Entries} ->
                    [filename:join(FolderPath, E) || E <- Entries,
                     filelib:is_regular(filename:join(FolderPath, E)),
                     has_extension(E, Extensions)];
                {error, _} ->
                    []
            end
    end.

has_extension(Filename, Extensions) ->
    Ext = string:lowercase(filename:extension(Filename)),
    lists:member(Ext, Extensions).

file_info(FilePath) ->
    case file:read_file_info(FilePath) of
        {ok, #file_info{size = Size}} ->
            #{
                <<"path">> => list_to_binary(FilePath),
                <<"name">> => list_to_binary(filename:basename(FilePath)),
                <<"size">> => Size,
                <<"extension">> => list_to_binary(filename:extension(FilePath))
            };
        {error, _} ->
            #{
                <<"path">> => list_to_binary(FilePath),
                <<"name">> => list_to_binary(filename:basename(FilePath)),
                <<"error">> => <<"Could not read file info">>
            }
    end.

run_analysis(Content, full) ->
    %% Run comprehensive analysis
    TextAnalysis = text_analyzer:analyze_text(Content),
    Models = maps:get(<<"models">>, TextAnalysis, []),
    TopModels = maps:get(<<"top_models">>, TextAnalysis, []),
    
    %% Check for Lollapalooza effect (3+ high-scoring models)
    HighScoring = [M || M <- Models, maps:get(<<"score">>, M, 0) >= 70],
    LollapaloozaDetected = length(HighScoring) >= 3,
    
    %% Get patterns
    Patterns = pattern_extractor:extract_patterns(Content),
    PatternList = maps:get(<<"patterns">>, Patterns, []),
    
    %% Get insights
    Insights = pattern_extractor:extract_key_insights(Content),
    InsightList = maps:get(<<"insights">>, Insights, []),
    
    #{
        <<"success">> => true,
        <<"models">> => TopModels,
        <<"all_models">> => Models,
        <<"model_count">> => length(Models),
        <<"high_scoring_count">> => length(HighScoring),
        <<"lollapalooza_detected">> => LollapaloozaDetected,
        <<"patterns">> => PatternList,
        <<"pattern_count">> => length(PatternList),
        <<"insights">> => InsightList,
        <<"insight_count">> => length(InsightList)
    };

run_analysis(Content, models) ->
    TextAnalysis = text_analyzer:analyze_text(Content),
    #{
        <<"success">> => true,
        <<"models">> => maps:get(<<"models">>, TextAnalysis, []),
        <<"top_models">> => maps:get(<<"top_models">>, TextAnalysis, [])
    };

run_analysis(Content, patterns) ->
    Patterns = pattern_extractor:extract_patterns(Content),
    #{
        <<"success">> => true,
        <<"patterns">> => maps:get(<<"patterns">>, Patterns, [])
    };

run_analysis(Content, quick) ->
    %% Quick analysis - just top models
    TextAnalysis = text_analyzer:analyze_text(Content),
    TopModels = maps:get(<<"top_models">>, TextAnalysis, []),
    #{
        <<"success">> => true,
        <<"models">> => TopModels,
        <<"model_count">> => length(TopModels)
    }.

count_models(Models) ->
    lists:foldl(fun(Model, Acc) ->
        Name = maps:get(<<"name">>, Model, <<"Unknown">>),
        maps:update_with(Name, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, Models).

top_n_models(ModelCounts, N) ->
    Sorted = lists:sort(fun({_, A}, {_, B}) -> A >= B end, maps:to_list(ModelCounts)),
    TopN = lists:sublist(Sorted, N),
    [#{<<"name">> => Name, <<"count">> => Count} || {Name, Count} <- TopN].
