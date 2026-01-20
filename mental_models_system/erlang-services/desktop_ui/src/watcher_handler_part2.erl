%%%-------------------------------------------------------------------
%%% @doc watcher_handler_part2 Template - Combined
%%% @end
%%%-------------------------------------------------------------------
-module(watcher_handler_part2).

-export([content/0]).

content() ->
    iolist_to_binary([
        watcher_handler_part2_chunk1:content(), watcher_handler_part2_chunk2:content(), watcher_handler_part2_chunk3:content()
    ]).
