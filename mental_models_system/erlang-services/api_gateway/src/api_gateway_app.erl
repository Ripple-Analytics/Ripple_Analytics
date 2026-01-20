%%%-------------------------------------------------------------------
%%% @doc Mental Models API Gateway Application
%%% 
%%% Main entry point for the API Gateway service.
%%% Routes requests to backend microservices with fault tolerance.
%%% @end
%%%-------------------------------------------------------------------
-module(api_gateway_app).
-behaviour(application).

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @doc Start the API Gateway application
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    Port = application:get_env(api_gateway, port, 8000),
    
    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/api/analysis/[...]", proxy_handler, [analysis]},
            {"/api/harvester/[...]", proxy_handler, [harvester]},
            {"/api/storage/[...]", proxy_handler, [storage]},
            {"/[...]", not_found_handler, []}
        ]}
    ]),
    
    %% Start Cowboy HTTP server
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch},
          middlewares => [cowboy_router, cors_middleware, cowboy_handler]}
    ),
    
    io:format("API Gateway started on port ~p~n", [Port]),
    api_gateway_sup:start_link().

%%--------------------------------------------------------------------
%% @doc Stop the API Gateway application
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok = cowboy:stop_listener(http_listener),
    ok.
