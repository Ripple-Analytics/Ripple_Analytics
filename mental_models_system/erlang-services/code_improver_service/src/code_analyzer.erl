-module(code_analyzer).

-export([
    scan_for_improvements/1,
    analyze_file/2,
    get_file_content/1,
    find_code_files/1,
    calculate_complexity/1,
    detect_code_smells/1
]).

scan_for_improvements(BasePath) ->
    io:format("[ANALYZER] Scanning for improvement opportunities in ~s~n", [BasePath]),
    Files = find_code_files(BasePath),
    io:format("[ANALYZER] Found ~p code files~n", [length(Files)]),
    
    Opportunities = lists:filtermap(
        fun(File) ->
            case analyze_file(File, BasePath) of
                {ok, Analysis} when map_get(improvement_score, Analysis) > 0.3 ->
                    {true, Analysis};
                _ ->
                    false
            end
        end,
        Files
    ),
    
    SortedOpportunities = lists:sort(
        fun(A, B) ->
            map_get(improvement_score, A) > map_get(improvement_score, B)
        end,
        Opportunities
    ),
    
    {ok, SortedOpportunities}.

find_code_files(BasePath) ->
    Extensions = [".erl", ".clj", ".cljc", ".cljs", ".py", ".js", ".ts"],
    ExcludeDirs = ["_build", "node_modules", ".git", "deps", "target"],
    
    case filelib:is_dir(BasePath) of
        true ->
            find_files_recursive(BasePath, Extensions, ExcludeDirs);
        false ->
            []
    end.

find_files_recursive(Dir, Extensions, ExcludeDirs) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:flatmap(
                fun(File) ->
                    FullPath = filename:join(Dir, File),
                    case filelib:is_dir(FullPath) of
                        true ->
                            case lists:member(File, ExcludeDirs) of
                                true -> [];
                                false -> find_files_recursive(FullPath, Extensions, ExcludeDirs)
                            end;
                        false ->
                            Ext = filename:extension(File),
                            case lists:member(Ext, Extensions) of
                                true -> [FullPath];
                                false -> []
                            end
                    end
                end,
                Files
            );
        {error, _} ->
            []
    end.

analyze_file(FilePath, BasePath) ->
    case get_file_content(FilePath) of
        {ok, Content} ->
            RelPath = string:prefix(FilePath, BasePath),
            Complexity = calculate_complexity(Content),
            CodeSmells = detect_code_smells(Content),
            LineCount = length(binary:split(Content, <<"\n">>, [global])),
            
            ImprovementScore = calculate_improvement_score(Complexity, CodeSmells, LineCount),
            
            {ok, #{
                file_path => list_to_binary(FilePath),
                relative_path => ensure_binary(RelPath),
                line_count => LineCount,
                complexity => Complexity,
                code_smells => CodeSmells,
                improvement_score => ImprovementScore,
                content => Content
            }};
        {error, Reason} ->
            {error, Reason}
    end.

get_file_content(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Content} -> {ok, Content};
        {error, Reason} -> {error, Reason}
    end.

calculate_complexity(Content) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    
    NestedDepth = calculate_nesting_depth(Lines),
    FunctionCount = count_functions(Content),
    ConditionCount = count_conditions(Content),
    
    BaseComplexity = ConditionCount + 1,
    NestingPenalty = NestedDepth * 0.5,
    SizePenalty = case length(Lines) of
        N when N > 500 -> 2.0;
        N when N > 200 -> 1.0;
        N when N > 100 -> 0.5;
        _ -> 0.0
    end,
    
    BaseComplexity + NestingPenalty + SizePenalty.

calculate_nesting_depth(Lines) ->
    lists:foldl(
        fun(Line, {MaxDepth, CurrentDepth}) ->
            OpenBraces = count_char(Line, ${),
            CloseBraces = count_char(Line, $}),
            NewDepth = CurrentDepth + OpenBraces - CloseBraces,
            {max(MaxDepth, NewDepth), max(0, NewDepth)}
        end,
        {0, 0},
        Lines
    ) |> element(1).

count_char(Binary, Char) ->
    length([C || <<C>> <= Binary, C == Char]).

count_functions(Content) ->
    Patterns = [
        <<"defn ">>, <<"defn- ">>, <<"def ">>,
        <<"fun ">>, <<"-spec ">>,
        <<"function ">>, <<"const ">>,
        <<"def ">>, <<"async def ">>
    ],
    lists:sum([count_pattern(Content, P) || P <- Patterns]).

count_conditions(Content) ->
    Patterns = [
        <<"if ">>, <<"if(">>,
        <<"case ">>, <<"cond ">>,
        <<"when ">>, <<"else ">>,
        <<"elif ">>, <<"switch ">>
    ],
    lists:sum([count_pattern(Content, P) || P <- Patterns]).

count_pattern(Content, Pattern) ->
    length(binary:matches(Content, Pattern)).

detect_code_smells(Content) ->
    Smells = [],
    
    Smells1 = case binary:match(Content, <<"TODO">>) of
        nomatch -> Smells;
        _ -> [{todo_comment, <<"Contains TODO comments">>} | Smells]
    end,
    
    Smells2 = case binary:match(Content, <<"FIXME">>) of
        nomatch -> Smells1;
        _ -> [{fixme_comment, <<"Contains FIXME comments">>} | Smells1]
    end,
    
    Smells3 = case binary:match(Content, <<"HACK">>) of
        nomatch -> Smells2;
        _ -> [{hack_comment, <<"Contains HACK comments">>} | Smells2]
    end,
    
    Lines = binary:split(Content, <<"\n">>, [global]),
    LongLines = [L || L <- Lines, byte_size(L) > 120],
    Smells4 = case length(LongLines) of
        0 -> Smells3;
        N -> [{long_lines, list_to_binary(io_lib:format("~p lines over 120 chars", [N]))} | Smells3]
    end,
    
    Smells5 = case count_pattern(Content, <<"catch _:_">>) + count_pattern(Content, <<"except:">>) + count_pattern(Content, <<"catch (">>) of
        0 -> Smells4;
        _ -> [{broad_exception, <<"Catches broad exceptions">>} | Smells4]
    end,
    
    Smells5.

calculate_improvement_score(Complexity, CodeSmells, LineCount) ->
    ComplexityScore = min(1.0, Complexity / 20.0),
    SmellScore = min(1.0, length(CodeSmells) / 5.0),
    SizeScore = min(1.0, LineCount / 500.0),
    
    (ComplexityScore * 0.4) + (SmellScore * 0.4) + (SizeScore * 0.2).

ensure_binary(false) -> <<"">>;
ensure_binary(B) when is_binary(B) -> B;
ensure_binary(L) when is_list(L) -> list_to_binary(L).
