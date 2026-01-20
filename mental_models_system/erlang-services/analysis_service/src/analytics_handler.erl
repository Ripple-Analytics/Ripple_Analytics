%%%-------------------------------------------------------------------
%%% @doc Analytics HTTP Handler
%%% 
%%% REST API for analytics and statistics.
%%% 
%%% Endpoints:
%%% GET /api/analysis/analytics - Get aggregated statistics
%%% GET /api/analysis/analytics/trends - Get trend data
%%% POST /api/analysis/analytics/reset - Reset statistics
%%% @end
%%%-------------------------------------------------------------------
-module(analytics_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    Req = handle_request(Method, Path, Req0),
    {ok, Req, State}.

handle_request(<<"GET">>, Path, Req) ->
    case binary:match(Path, <<"/trends">>) of
        nomatch ->
            handle_get_stats(Req);
        _ ->
            handle_get_trends(Req)
    end;

handle_request(<<"POST">>, Path, Req0) ->
    case binary:match(Path, <<"/reset">>) of
        nomatch ->
            json_response(400, #{<<"error">> => <<"Unknown action">>}, Req0);
        _ ->
            handle_reset(Req0)
    end;

handle_request(_, _, Req) ->
    json_response(405, #{<<"error">> => <<"Method not allowed">>}, Req).

handle_get_stats(Req) ->
    case analytics_service:get_stats() of
        {ok, Stats} ->
            json_response(200, Stats, Req);
        {error, Reason} ->
            json_response(500, #{<<"error">> => format_error(Reason)}, Req)
    end.

handle_get_trends(Req) ->
    case analytics_service:get_trends() of
        {ok, Trends} ->
            json_response(200, Trends, Req);
        {error, Reason} ->
            json_response(500, #{<<"error">> => format_error(Reason)}, Req)
    end.

handle_reset(Req) ->
    case analytics_service:reset_stats() of
        {ok, Result} ->
            json_response(200, Result, Req);
        {error, Reason} ->
            json_response(500, #{<<"error">> => format_error(Reason)}, Req)
    end.

json_response(Status, Body, Req) ->
    cowboy_req:reply(Status, #{
        <<"content-type">> => <<"application/json">>,
        <<"access-control-allow-origin">> => <<"*">>
    }, jsx:encode(Body), Req).

format_error(Reason) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
format_error(Reason) when is_binary(Reason) -> Reason;
format_error(Reason) -> list_to_binary(io_lib:format("~p", [Reason])).
