%%%-------------------------------------------------------------------
%%% @doc Health Check Handler
%%% 
%%% Handles health check requests for the Analysis Service.
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
    LlmAvailable = llm_client:is_available(),
    Models = model_registry:get_all_models(),
    
    Response = jsx:encode(#{
        <<"status">> => <<"healthy">>,
        <<"service">> => <<"analysis-service">>,
        <<"version">> => <<"1.0.0">>,
        <<"llm_available">> => LlmAvailable,
        <<"models_loaded">> => length(Models),
        <<"erlang_version">> => list_to_binary(erlang:system_info(otp_release))
    }),
    
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>},
        Response,
        Req0),
    
    {ok, Req, State}.
