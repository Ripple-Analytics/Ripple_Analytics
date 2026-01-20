%%%-------------------------------------------------------------------
%%% @doc Document Handler - Get, update, delete single document
%%%-------------------------------------------------------------------
-module(document_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Id = cowboy_req:binding(id, Req0),
    case cowboy_req:method(Req0) of
        <<"GET">> ->
            case document_store:get(Id) of
                {ok, Doc} ->
                    Req = cowboy_req:reply(200, cors_headers(), jsx:encode(Doc), Req0),
                    {ok, Req, State};
                {error, not_found} ->
                    Req = cowboy_req:reply(404, cors_headers(),
                        jsx:encode(#{<<"error">> => <<"Not found">>}), Req0),
                    {ok, Req, State}
            end;
        <<"DELETE">> ->
            document_store:delete(Id),
            Req = cowboy_req:reply(204, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"GET, DELETE, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
