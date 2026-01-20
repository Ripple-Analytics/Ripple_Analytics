-module(lm_self_heal_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            handle_heal(Body, Req1, State);
        <<"GET">> ->
            %% Return healer status
            {ok, Status} = self_healer:get_status(),
            Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
                jsx:encode(Status), Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_heal(Body, Req0, State) ->
    try jsx:decode(Body, [return_maps]) of
        Params ->
            Action = maps:get(<<"action">>, Params, <<"check">>),
            
            Result = case Action of
                <<"check">> ->
                    self_healer:check_all_services();
                <<"heal">> ->
                    ServiceName = maps:get(<<"service">>, Params, <<>>),
                    self_healer:heal_service(ServiceName);
                _ ->
                    {error, unknown_action}
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
