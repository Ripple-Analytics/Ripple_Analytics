%%%-------------------------------------------------------------------
%%% @doc Autonomous Analyzer - Codebase analysis for improvement opportunities
%%% @end
%%%-------------------------------------------------------------------
-module(autonomous_analyzer).

-export([analyze_codebase/0, analyze_file/1, has_issues/1]).

%%====================================================================
%% API
%%====================================================================

analyze_codebase() ->
    BasePath = "/repo/mental_models_system/erlang-services",
    ErlFiles = filelib:wildcard(BasePath ++ "/**/src/*.erl"),
    Analysis = lists:map(fun analyze_file/1, ErlFiles),
    #{
        <<"total_files">> => length(ErlFiles),
        <<"files_with_issues">> => length([A || A <- Analysis, has_issues(A)]),
        <<"improvement_opportunities">> => extract_opportunities(Analysis),
        <<"file_analysis">> => Analysis
    }.

analyze_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            Lines = binary:split(Content, <<"\n">>, [global]),
            #{
                <<"file">> => list_to_binary(FilePath),
                <<"lines">> => length(Lines),
                <<"has_todos">> => has_pattern(Content, <<"TODO">>),
                <<"has_fixmes">> => has_pattern(Content, <<"FIXME">>),
                <<"missing_docs">> => not has_pattern(Content, <<"@doc">>),
                <<"long_functions">> => count_long_functions(Content),
                <<"complexity_score">> => estimate_complexity(Content)
            };
        {error, _} ->
            #{<<"file">> => list_to_binary(FilePath), <<"error">> => <<"read_failed">>}
    end.

has_issues(#{<<"has_todos">> := true}) -> true;
has_issues(#{<<"has_fixmes">> := true}) -> true;
has_issues(#{<<"missing_docs">> := true}) -> true;
has_issues(#{<<"long_functions">> := N}) when N > 0 -> true;
has_issues(_) -> false.

%%====================================================================
%% Internal
%%====================================================================

has_pattern(Content, Pattern) ->
    binary:match(Content, Pattern) =/= nomatch.

count_long_functions(Content) ->
    Functions = binary:split(Content, <<".\n">>, [global]),
    length([F || F <- Functions, byte_size(F) > 2000]).

estimate_complexity(Content) ->
    CaseCount = length(binary:matches(Content, <<"case">>)),
    IfCount = length(binary:matches(Content, <<"if">>)),
    TryCount = length(binary:matches(Content, <<"try">>)),
    CaseCount + IfCount + TryCount.

extract_opportunities(Analysis) ->
    lists:filtermap(fun(A) ->
        case has_issues(A) of
            true -> {true, maps:get(<<"file">>, A, <<>>)};
            false -> false
        end
    end, Analysis).
