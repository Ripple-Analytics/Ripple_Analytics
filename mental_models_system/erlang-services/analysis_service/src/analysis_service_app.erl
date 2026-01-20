%%%-------------------------------------------------------------------
%%% @doc Mental Models Analysis Service Application
%%% 
%%% Provides mental model analysis, pattern detection, and LLM integration.
%%% Built with OTP for maximum reliability.
%%% @end
%%%-------------------------------------------------------------------
-module(analysis_service_app).
-behaviour(application).

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @doc Start the Analysis Service application
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    Port = application:get_env(analysis_service, port, 8001),
    
    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/api/analysis/models", models_handler, []},
            {"/api/analysis/categories", categories_handler, []},
            {"/api/analysis/analyze", analyze_handler, []},
            {"/api/analysis/comprehensive", comprehensive_handler, []},
            {"/api/analysis/bayesian", bayesian_handler, []},
            {"/api/analysis/patterns", patterns_handler, []},
            {"/api/analysis/detect-biases", biases_handler, []},
            {"/api/analysis/folder", folder_scraper_handler, []},
            {"/api/analysis/watcher", folder_watcher_handler, []},
            {"/api/analysis/report", report_handler, []},
            {"/[...]", not_found_handler, []}
        ]}
    ]),
    
    %% Start Cowboy HTTP server
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("Analysis Service started on port ~p~n", [Port]),
    analysis_service_sup:start_link().

%%--------------------------------------------------------------------
%% @doc Stop the Analysis Service application
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok = cowboy:stop_listener(http_listener),
    ok.
