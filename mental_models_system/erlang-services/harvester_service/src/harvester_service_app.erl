%%%-------------------------------------------------------------------
%%% @doc Mental Models Harvester Service Application
%%% 
%%% Web scraping and data collection service with OTP supervision.
%%% @end
%%%-------------------------------------------------------------------
-module(harvester_service_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Port = application:get_env(harvester_service, port, 8002),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/api/harvester/scrape", scrape_handler, []},
            {"/api/harvester/batch-scrape", batch_scrape_handler, []},
            {"/api/harvester/scrape-analyze", scrape_analyze_handler, []},
            {"/api/harvester/process", process_handler, []},
            {"/api/harvester/stats", stats_handler, []},
            {"/[...]", not_found_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("Harvester Service started on port ~p~n", [Port]),
    harvester_service_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http_listener),
    ok.
