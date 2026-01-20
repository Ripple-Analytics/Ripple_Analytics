%%%-------------------------------------------------------------------
%%% @doc report_generator Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(report_generator_part2).

-export([generate_batch_analysis_content/1, stat_box/2, model_tag/1, model_tag/1, model_count_tag/1, file_result_html/1, format_timestamp/0, ensure_binary/1, ensure_binary/1, ensure_binary/1, ensure_binary/1, ensure_binary/1]).

generate_batch_analysis_content(Result) ->
    Results = maps:get(<<"results">>, Result, []),
    TotalFiles = maps:get(<<"total_files">>, Result, length(Results)),
    Analyzed = maps:get(<<"analyzed">>, Result, TotalFiles),
    Failed = maps:get(<<"failed">>, Result, 0),
    Summary = maps:get(<<"summary">>, Result, #{}),
    TopModels = maps:get(<<"top_models">>, Summary, []),
    LollapaloozaFiles = maps:get(<<"lollapalooza_files">>, Summary, []),
    LollapaloozaCount = maps:get(<<"lollapalooza_count">>, Summary, length(LollapaloozaFiles)),
    
    [
        case LollapaloozaCount > 0 of
            true ->
                [<<"<div class='lollapalooza'><h3>LOLLAPALOOZA EFFECTS DETECTED</h3>">>,
                 <<"<p>Found in ">>, integer_to_binary(LollapaloozaCount), <<" files: ">>,
                 lists:join(<<", ">>, LollapaloozaFiles),
                 <<"</p></div>">>];
            false -> <<>>
        end,
        
        <<"<div class='card'><h2>Batch Summary</h2><div class='stat-grid'>">>,
        stat_box(<<"Total Files">>, TotalFiles),
        stat_box(<<"Analyzed">>, Analyzed),
        stat_box(<<"Failed">>, Failed),
        stat_box(<<"Lollapalooza">>, LollapaloozaCount),
        <<"</div></div>">>,
        
        case TopModels of
            [] -> <<>>;
            _ ->
                [<<"<div class='card'><h2>Top Models Across All Files</h2><div class='model-list'>">>,
                 [model_count_tag(M) || M <- lists:sublist(TopModels, 10)],
                 <<"</div></div>">>]
        end,
        
        <<"<div class='card'><h2>Individual File Results</h2>">>,
        [file_result_html(R) || R <- Results],
        <<"</div>">>
    ].

stat_box(Label, Value) ->
    [<<"<div class='stat'><div class='stat-value'>">>,
     integer_to_binary(Value),
     <<"</div><div class='stat-label'>">>,
     Label,
     <<"</div></div>">>].

model_tag(Model) when is_map(Model) ->
    Name = maps:get(<<"name">>, Model, <<"Unknown">>),
    Score = maps:get(<<"score">>, Model, 0),
    Class = case Score >= 70 of true -> <<"model-tag high">>; false -> <<"model-tag">> end,
    [<<"<span class='">>, Class, <<"'>">>, Name, <<"</span>">>];
model_tag(Name) when is_binary(Name) ->
    [<<"<span class='model-tag'>">>, Name, <<"</span>">>].

model_count_tag(Model) when is_map(Model) ->
    Name = maps:get(<<"name">>, Model, <<"Unknown">>),
    Count = maps:get(<<"count">>, Model, 1),
    [<<"<span class='model-tag'>">>, Name, <<" (">>, integer_to_binary(Count), <<")</span>">>].

file_result_html(Result) ->
    FileName = maps:get(<<"file_name">>, Result, maps:get(<<"file">>, Result, <<"Unknown">>)),
    Success = maps:get(<<"success">>, Result, false),
    Models = maps:get(<<"models">>, Result, []),
    LollapaloozaDetected = maps:get(<<"lollapalooza_detected">>, Result, false),
    
    [<<"<div class='file-result'><h4>">>, ensure_binary(FileName), <<"</h4>">>,
     case Success of
         true ->
             [case LollapaloozaDetected of
                  true -> <<"<span class='model-tag high'>LOLLAPALOOZA</span> ">>;
                  false -> <<>>
              end,
              <<"<div class='model-list' style='margin-top:8px'>">>,
              [model_tag(M) || M <- lists:sublist(Models, 5)],
              <<"</div>">>];
         false ->
             Error = maps:get(<<"error">>, Result, <<"Analysis failed">>),
             [<<"<span style='color:#ef4444'>Error: ">>, ensure_binary(Error), <<"</span>">>]
     end,
     <<"</div>">>].

format_timestamp() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:local_time(),
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y, M, D, H, Mi, S])).

ensure_binary(B) when is_binary(B) -> B;
ensure_binary(L) when is_list(L) -> list_to_binary(L);
ensure_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
ensure_binary(I) when is_integer(I) -> integer_to_binary(I);
ensure_binary(Other) -> list_to_binary(io_lib:format("~p", [Other])).

