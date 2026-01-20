%%%-------------------------------------------------------------------
%%% @doc Health Check Handler
%%% 
%%% Handles health check requests for the API Gateway.
%%% @end
%%%-------------------------------------------------------------------
-module(health_handler).
-behaviour(cowboy_handler).

-export([init/2]).

%%--------------------------------------------------------------------
%% @doc Handle health check request
%% @end
%%--------------------------------------------------------------------
init(Req0, State) ->
    Response = jsx:encode(#{
        <<"status">> => <<"healthy">>,
        <<"service">> => <<"api-gateway">>,
        <<"version">> => <<"1.0.0">>,
        <<"erlang_version">> => list_to_binary(erlang:system_info(otp_release))
    }),
    
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>},
        Response,
        Req0),
    
    {ok, Req, State}.
