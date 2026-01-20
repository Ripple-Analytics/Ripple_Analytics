-module(improver_start_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    case improvement_scheduler:start_cycle() of
        ok ->
            Response = jsx:encode(#{<<"status">> => <<"started">>, <<"message">> => <<"Improvement cycle started">>}),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Response,
                Req0);
        {error, already_running} ->
            Response = jsx:encode(#{<<"status">> => <<"already_running">>, <<"message">> => <<"Improvement cycle already running">>}),
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
