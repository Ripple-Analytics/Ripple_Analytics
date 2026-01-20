%%%-------------------------------------------------------------------
%%% @doc History Stats Handler - Analysis statistics endpoint
%%%-------------------------------------------------------------------
-module(history_stats_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> ->
            handle_stats(Req0, State);
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_stats(Req0, State) ->
    {ok, Stats} = history_store:get_stats(),
    Response = jsx:encode(#{
        <<"success">> => true,
        <<"stats">> => Stats
    }),
    Req = cowboy_req:reply(200, cors_headers(), Response, Req0),
    {ok, Req, State}.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"GET, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
