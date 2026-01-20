-module(lm_analyze_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            handle_analyze(Body, Req1, State);
        <<"GET">> ->
            %% Return UI analysis
            {ok, Analysis} = ui_customizer:analyze_ui(),
            Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
                jsx:encode(Analysis), Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_analyze(Body, Req0, State) ->
    try jsx:decode(Body, [return_maps]) of
        Params ->
            Target = maps:get(<<"target">>, Params, <<"codebase">>),
            
            Result = case Target of
                <<"ui">> ->
                    ui_customizer:analyze_ui();
                <<"codebase">> ->
                    %% Trigger autonomous engine analysis
                    autonomous_engine:force_cycle();
                <<"handler">> ->
                    HandlerName = maps:get(<<"handler">>, Params, <<>>),
                    ui_customizer:improve_handler(HandlerName);
                _ ->
                    {error, unknown_target}
            end,
            
            Response = case Result of
                {ok, Data} ->
                    jsx:encode(#{<<"success">> => true, <<"data">> => Data});
                {error, Reason} ->
                    jsx:encode(#{<<"success">> => false, 
                                 <<"error">> => list_to_binary(io_lib:format("~p", [Reason]))})
            end,
            
            Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
                Response, Req0),
            {ok, Req, State}
    catch
        _:_ ->
            Req = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Invalid JSON">>}), Req0),
            {ok, Req, State}
    end.
