%%%-------------------------------------------------------------------
%%% @doc Detector Utilities - Shared functions for model detection
%%% @end
%%%-------------------------------------------------------------------
-module(detector_utils).

-export([count_keywords/2, count_patterns/2, count_numbers/1, 
         confidence_level/1, make_result/5]).

%% @doc Count occurrences of keywords in text
count_keywords(Text, Keywords) ->
    lists:foldl(fun(Keyword, Acc) ->
        case binary:match(Text, Keyword) of
            nomatch -> Acc;
            _ -> Acc + 1
        end
    end, 0, Keywords).

%% @doc Count occurrences of patterns in text
count_patterns(Text, Patterns) ->
    lists:foldl(fun(Pattern, Acc) ->
        case binary:match(Text, Pattern) of
            nomatch -> Acc;
            _ -> Acc + 1
        end
    end, 0, Patterns).

%% @doc Count numbers in text
count_numbers(Text) ->
    Matches = binary:matches(Text, [<<"0">>, <<"1">>, <<"2">>, <<"3">>, <<"4">>,
                                     <<"5">>, <<"6">>, <<"7">>, <<"8">>, <<"9">>]),
    length(Matches).

%% @doc Determine confidence level from score
confidence_level(Score) when Score >= 0.7 -> <<"High">>;
confidence_level(Score) when Score >= 0.4 -> <<"Moderate">>;
confidence_level(_) -> <<"Low">>.

%% @doc Create a standard result map
make_result(Name, Category, Score, Evidence, Slug) ->
    #{
        <<"name">> => Name,
        <<"category">> => Category,
        <<"model_slug">> => Slug,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => Evidence,
        <<"confidence">> => confidence_level(Score)
    }.
