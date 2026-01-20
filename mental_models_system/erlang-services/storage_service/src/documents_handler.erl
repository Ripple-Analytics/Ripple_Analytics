%%%-------------------------------------------------------------------
%%% @doc Documents Handler - List and create documents
%%%-------------------------------------------------------------------
-module(documents_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> ->
            {ok, Docs} = document_store:list_all(),
            Req = cowboy_req:reply(200, cors_headers(),
                jsx:encode(#{<<"documents">> => Docs, <<"count">> => length(Docs)}), Req0),
            {ok, Req, State};
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            try jsx:decode(Body, [return_maps]) of
                Doc ->
                    Id = maps:get(<<"id">>, Doc, generate_id()),
                    {ok, Stored} = document_store:store(Id, Doc),
                    Req2 = cowboy_req:reply(201, cors_headers(), jsx:encode(Stored), Req1),
                    {ok, Req2, State}
            catch _:_ ->
                Req3 = cowboy_req:reply(400, cors_headers(),
                    jsx:encode(#{<<"error">> => <<"Invalid JSON">>}), Req1),
                {ok, Req3, State}
            end;
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

generate_id() ->
    list_to_binary(integer_to_list(erlang:unique_integer([positive]))).

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"GET, POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
