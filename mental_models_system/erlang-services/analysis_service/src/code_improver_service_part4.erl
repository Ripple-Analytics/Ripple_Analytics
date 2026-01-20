%%%-------------------------------------------------------------------
%%% @doc code_improver_service Helper Module - Part 4
%%% @end
%%%-------------------------------------------------------------------
-module(code_improver_service_part4).

-export([format_datetime/6]).

format_datetime({{Y, M, D}, {H, Mi, S}}) ->
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", 
                                  [Y, M, D, H, Mi, S])).

