-module(improver_status_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    case improvement_scheduler:get_status() of
        {ok, Status} ->
            case lm_studio_client:check_connection() of
                {ok, connected} ->
                    FullStatus = Status#{<<"lm_studio">> => <<"connected">>};
                {error, _} ->
                    FullStatus = Status#{<<"lm_studio">> => <<"disconnected">>}
            end,
            Response = jsx:encode(FullStatus),
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
