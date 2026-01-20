-module(lm_improve_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            handle_improve(Req0, State);
        <<"GET">> ->
            handle_status(Req0, State);
        _ ->
            Req = cowboy_req:reply(405, #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_improve(Req0, State) ->
    %% Trigger an improvement cycle
    Result = autonomous_engine:force_cycle(),
    
    Response = case Result of
        {ok, CycleInfo} ->
            jsx:encode(#{<<"success">> => true, <<"cycle">> => CycleInfo});
        {error, Reason} ->
            jsx:encode(#{<<"success">> => false, 
                         <<"error">> => list_to_binary(io_lib:format("~p", [Reason]))})
    end,
    
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
        Response, Req0),
    {ok, Req, State}.

handle_status(Req0, State) ->
    {ok, Status} = autonomous_engine:get_status(),
    
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Status), Req0),
    {ok, Req, State}.
