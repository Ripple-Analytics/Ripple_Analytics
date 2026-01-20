-module(http_test_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),
    SourceName = maps:get(<<"source">>, Data, <<"primary">>),
    
    case http_worker:test_source(SourceName) of
        {ok, Result} ->
            Response = jsx:encode(#{success => true, test => Result}),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Response,
                Req1);
        {error, Reason} ->
            Response = jsx:encode(#{success => false, error => atom_to_binary(Reason, utf8)}),
            Req = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                Response,
                Req1)
    end,
    {ok, Req, State}.
