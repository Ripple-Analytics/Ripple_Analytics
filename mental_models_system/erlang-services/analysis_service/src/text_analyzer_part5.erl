%%%-------------------------------------------------------------------
%%% @doc text_analyzer Helper Module - Part 5
%%% @end
%%%-------------------------------------------------------------------
-module(text_analyzer_part5).

-export([detect_second_order_thinking/1, count_keywords/2, count_patterns/2, count_numbers/1, confidence_level/1, confidence_level/1, confidence_level/1]).

detect_second_order_thinking(Text) ->
    Keywords = [<<"consequences">>, <<"effects">>, <<"long-term">>, <<"ripple">>,
                <<"cascade">>, <<"downstream">>, <<"implications">>, <<"then what">>],
    
    TextLower = string:lowercase(Text),
    KeywordCount = count_keywords(TextLower, Keywords),
    
    PatternCount = count_patterns(TextLower, [
        <<"second order">>, <<"and then">>, <<"what happens next">>,
        <<"long term">>, <<"downstream effect">>
    ]),
    
    BaseScore = (KeywordCount * 0.04) + (PatternCount * 0.2),
    Score = min(1.0, BaseScore),
    
    #{
        <<"name">> => <<"Second-Order Thinking">>,
        <<"category">> => <<"Thinking Tools">>,
        <<"score">> => round(Score * 100),
        <<"relevance">> => round(Score * 100),
        <<"evidence">> => #{<<"keywords">> => KeywordCount, <<"patterns">> => PatternCount},
        <<"confidence">> => confidence_level(Score)
    }.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%% Count occurrences of keywords in text
count_keywords(Text, Keywords) ->
    lists:foldl(fun(Keyword, Acc) ->
        case binary:match(Text, Keyword) of
            nomatch -> Acc;
            _ -> Acc + 1
        end
    end, 0, Keywords).

%% Count occurrences of patterns in text
count_patterns(Text, Patterns) ->
    lists:foldl(fun(Pattern, Acc) ->
        case binary:match(Text, Pattern) of
            nomatch -> Acc;
            _ -> Acc + 1
        end
    end, 0, Patterns).

%% Count numbers in text
count_numbers(Text) ->
    %% Simple count of digit sequences
    Matches = binary:matches(Text, [<<"0">>, <<"1">>, <<"2">>, <<"3">>, <<"4">>,
                                     <<"5">>, <<"6">>, <<"7">>, <<"8">>, <<"9">>]),
    length(Matches).

%% Determine confidence level from score
confidence_level(Score) when Score >= 0.7 -> <<"High">>;
confidence_level(Score) when Score >= 0.4 -> <<"Moderate">>;
confidence_level(_) -> <<"Low">>.

