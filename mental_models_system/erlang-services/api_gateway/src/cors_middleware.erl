%%%-------------------------------------------------------------------
%%% @doc CORS Middleware
%%% 
%%% Handles CORS preflight requests and adds CORS headers to responses.
%%% @end
%%%-------------------------------------------------------------------
-module(cors_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

%%--------------------------------------------------------------------
%% @doc Execute CORS middleware
%% @end
%%--------------------------------------------------------------------
execute(Req, Env) ->
    Method = cowboy_req:method(Req),
    
    case Method of
        <<"OPTIONS">> ->
            %% Handle preflight request
            Req2 = cowboy_req:reply(200, #{
                <<"access-control-allow-origin">> => <<"*">>,
                <<"access-control-allow-methods">> => <<"GET, POST, PUT, DELETE, OPTIONS">>,
                <<"access-control-allow-headers">> => <<"Content-Type, Authorization">>,
                <<"access-control-max-age">> => <<"86400">>
            }, <<>>, Req),
            {stop, Req2};
        _ ->
            %% Continue with request
            {ok, Req, Env}
    end.
