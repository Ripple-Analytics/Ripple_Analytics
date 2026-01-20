-module(improver_history_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    QsVals = cowboy_req:parse_qs(Req0),
    Limit = case lists:keyfind(<<"limit">>, 1, QsVals) of
        {_, LimitBin} -> binary_to_integer(LimitBin);
        false -> 100
    end,
    
    case improvement_history:get_history(Limit) of
        {ok, History} ->
            Response = jsx:encode(#{
                <<"count">> => length(History),
                <<"improvements">> => History
            }),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Response,
                Req0);
        {error, Reason} ->
            Response = jsx:encode(#{<<"error">> => list_to_binary(io_lib:format("~p", [Reason]))}),
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                Response,
                Req0)
    end,
    {ok, Req, State}.
