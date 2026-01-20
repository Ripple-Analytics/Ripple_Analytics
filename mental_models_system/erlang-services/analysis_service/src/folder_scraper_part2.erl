%%%-------------------------------------------------------------------
%%% @doc folder_scraper Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(folder_scraper_part2).

-export([find_files/3, has_extension/2, file_info/1, run_analysis/2, run_analysis/2, run_analysis/2, run_analysis/2, count_models/1, top_n_models/2]).

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

