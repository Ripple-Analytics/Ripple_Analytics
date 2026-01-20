%%%-------------------------------------------------------------------
%%% @doc Search Handler - Search documents
%%%-------------------------------------------------------------------
-module(search_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    #{q := Query} = cowboy_req:match_qs([{q, [], <<>>}], Req0),
    case Query of
        <<>> ->
            Req = cowboy_req:reply(400, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Query parameter 'q' required">>}), Req0),
            {ok, Req, State};
        _ ->
            {ok, Results} = document_store:search(Query),
            Req = cowboy_req:reply(200, cors_headers(),
                jsx:encode(#{<<"results">> => Results, <<"count">> => length(Results)}), Req0),
            {ok, Req, State}
    end.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>}.
