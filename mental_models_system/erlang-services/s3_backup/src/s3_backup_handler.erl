-module(s3_backup_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    case s3_worker:create_backup() of
        {ok, Result} ->
            Response = jsx:encode(#{success => true, backup => Result}),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Response,
                Req0);
        {error, Reason} ->
            Response = jsx:encode(#{success => false, error => atom_to_binary(Reason, utf8)}),
            Req = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                Response,
                Req0)
    end,
    {ok, Req, State}.
