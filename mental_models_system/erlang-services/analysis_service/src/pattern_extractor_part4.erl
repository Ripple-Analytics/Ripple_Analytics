%%%-------------------------------------------------------------------
%%% @doc pattern_extractor Helper Module - Part 4
%%% @end
%%%-------------------------------------------------------------------
-module(pattern_extractor_part4).

-export([has_uncertainty_pattern/1, has_absolute_pattern/1, has_comparison_pattern/1]).

has_uncertainty_pattern(Text) ->
    string:find(Text, "maybe") =/= nomatch orelse
    string:find(Text, "might") =/= nomatch orelse
    string:find(Text, "could") =/= nomatch orelse
    string:find(Text, "perhaps") =/= nomatch.

has_absolute_pattern(Text) ->
    string:find(Text, "always") =/= nomatch orelse
    string:find(Text, "never") =/= nomatch orelse
    string:find(Text, "everyone") =/= nomatch orelse
    string:find(Text, "no one") =/= nomatch.

has_comparison_pattern(Text) ->
    string:find(Text, "better") =/= nomatch orelse
    string:find(Text, "worse") =/= nomatch orelse
    string:find(Text, "more than") =/= nomatch orelse
    string:find(Text, "less than") =/= nomatch.

