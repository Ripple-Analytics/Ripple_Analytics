%%%-------------------------------------------------------------------
%%% @doc Not Found Handler
%%% 
%%% Handles requests to unknown endpoints.
%%% @end
%%%-------------------------------------------------------------------
-module(not_found_handler).
-behaviour(cowboy_handler).

-export([init/2]).

%%--------------------------------------------------------------------
%% @doc Handle not found request
%% @end
%%--------------------------------------------------------------------
init(Req0, State) ->
    Response = jsx:encode(#{
        <<"error">> => <<"Not Found">>,
        <<"message">> => <<"The requested endpoint does not exist">>
    }),
    
    Req = cowboy_req:reply(404,
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>},
        Response,
        Req0),
    
    {ok, Req, State}.
