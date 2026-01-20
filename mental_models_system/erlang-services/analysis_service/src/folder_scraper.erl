%%%-------------------------------------------------------------------
%%% @doc Folder Scraper - Batch File Analysis
%%% 
%%% Automatically scans a folder for text files and analyzes them
%%% for mental models, patterns, and biases.
%%% @end
%%%-------------------------------------------------------------------
-module(folder_scraper).
%% Helper modules: folder_scraper_part2

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
