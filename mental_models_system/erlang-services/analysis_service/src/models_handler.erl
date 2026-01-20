%%%-------------------------------------------------------------------
%%% @doc Models Handler
%%% 
%%% Handles requests for listing mental models.
%%% @end
%%%-------------------------------------------------------------------
-module(models_handler).
-behaviour(cowboy_handler).

-export([init/2]).

%%--------------------------------------------------------------------
%% @doc Handle models list request
%% @end
%%--------------------------------------------------------------------
init(Req0, State) ->
    Models = model_registry:get_all_models(),
    
    %% Convert records to maps for JSON encoding
    ModelMaps = lists:map(fun(Model) ->
        #{
            <<"id">> => element(2, Model),
            <<"name">> => element(3, Model),
            <<"category">> => element(4, Model),
            <<"description">> => element(5, Model),
            <<"key_insight">> => element(6, Model),
            <<"application">> => element(7, Model),
            <<"failure_modes">> => element(8, Model),
            <<"keywords">> => element(9, Model)
        }
    end, Models),
    
    Response = jsx:encode(#{
        <<"models">> => ModelMaps,
        <<"count">> => length(ModelMaps)
    }),
    
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>},
        Response,
        Req0),
    
    {ok, Req, State}.
