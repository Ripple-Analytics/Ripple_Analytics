%%%-------------------------------------------------------------------
%%% @doc History Item Handler - Single analysis history item endpoints
%%%-------------------------------------------------------------------
-module(history_item_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Id = cowboy_req:binding(id, Req0),
    case cowboy_req:method(Req0) of
        <<"GET">> ->
            handle_get(Id, Req0, State);
        <<"DELETE">> ->
            handle_delete(Id, Req0, State);
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_get(Id, Req0, State) ->
    case history_store:get_analysis(Id) of
        {ok, Analysis} ->
            Response = jsx:encode(#{
                <<"success">> => true,
                <<"analysis">> => Analysis
            }),
            Req = cowboy_req:reply(200, cors_headers(), Response, Req0),
            {ok, Req, State};
        {error, not_found} ->
            Req = cowboy_req:reply(404, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Analysis not found">>}), Req0),
            {ok, Req, State}
    end.

handle_delete(Id, Req0, State) ->
    ok = history_store:delete_analysis(Id),
    Response = jsx:encode(#{<<"success">> => true}),
    Req = cowboy_req:reply(200, cors_headers(), Response, Req0),
    {ok, Req, State}.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"GET, DELETE, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
