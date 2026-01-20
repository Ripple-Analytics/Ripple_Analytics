%%%-------------------------------------------------------------------
%%% @doc folder_watcher Helper Module - Part 3
%%% @end
%%%-------------------------------------------------------------------
-module(folder_watcher_part3).

-export([notify_file_analyzed/2, format_datetime/1, format_datetime/6]).

notify_file_analyzed(FileName, Result) ->
    LollapaloozaDetected = maps:get(<<"lollapalooza_detected">>, Result, false),
    Models = maps:get(<<"models">>, Result, []),
    TopModels = lists:sublist([maps:get(<<"name">>, M, <<"Unknown">>) || M <- Models], 3),
    
    case LollapaloozaDetected of
        true ->
            notification_service:notify(lollapalooza, #{
                <<"file">> => FileName,
                <<"message">> => <<"Lollapalooza effect detected!">>,
                <<"models">> => TopModels,
                <<"severity">> => <<"high">>
            });
        false when length(Models) > 0 ->
            notification_service:notify(analysis_complete, #{
                <<"file">> => FileName,
                <<"message">> => <<"Analysis complete">>,
                <<"models_found">> => length(Models),
                <<"top_models">> => TopModels
            });
        _ ->
            notification_service:notify(analysis_complete, #{
                <<"file">> => FileName,
                <<"message">> => <<"Analysis complete - no models detected">>
            })
    end.

format_datetime(undefined) -> <<"never">>;
format_datetime({{Y, M, D}, {H, Mi, S}}) ->
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", 
                                  [Y, M, D, H, Mi, S])).

