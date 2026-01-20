-module(autonomous_deployer_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    case improvement_scheduler:force_improvement() of
        {ok, Improvements} ->
            Response = jsx:encode(#{
                <<"status">> => <<"success">>,
                <<"improvements_deployed">> => length(Improvements),
                <<"improvements">> => Improvements
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
