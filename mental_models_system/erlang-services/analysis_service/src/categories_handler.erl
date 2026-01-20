%%%-------------------------------------------------------------------
%%% @doc Categories Handler
%%% 
%%% Handles requests for listing model categories.
%%% @end
%%%-------------------------------------------------------------------
-module(categories_handler).
-behaviour(cowboy_handler).

-export([init/2]).

%%--------------------------------------------------------------------
%% @doc Handle categories list request
%% @end
%%--------------------------------------------------------------------
init(Req0, State) ->
    Categories = model_registry:get_categories(),
    
    Response = jsx:encode(#{
        <<"categories">> => Categories,
        <<"count">> => length(Categories)
    }),
    
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>},
        Response,
        Req0),
    
    {ok, Req, State}.
