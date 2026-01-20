-module(metrics_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"GET">> ->
            handle_get_metrics(Req0, State);
        <<"DELETE">> ->
            handle_reset_metrics(Req0, State);
        _ ->
            Req = cowboy_req:reply(405,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}),
                Req0),
            {ok, Req, State}
    end.

handle_get_metrics(Req0, State) ->
    case improvement_metrics:get_metrics() of
        {ok, Metrics} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(Metrics),
                Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => format_error(Reason)}),
                Req0)
    end,
    {ok, Req, State}.

handle_reset_metrics(Req0, State) ->
    case improvement_metrics:reset_metrics() of
        ok ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"status">> => <<"reset">>}),
                Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => format_error(Reason)}),
                Req0)
    end,
    {ok, Req, State}.

format_error(Reason) when is_binary(Reason) -> Reason;
format_error(Reason) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
format_error(Reason) -> list_to_binary(io_lib:format("~p", [Reason])).
