%%%-------------------------------------------------------------------
%%% @doc Desktop UI Application - Web-based interface for Mental Models
%%% 
%%% Serves a light-mode responsive web UI that connects to backend
%%% microservices via the API Gateway.
%%% @end
%%%-------------------------------------------------------------------
-module(desktop_ui_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Port = application:get_env(desktop_ui, port, 3000),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", index_handler, []},
            {"/health", health_handler, []},
            {"/dashboard", dashboard_handler, []},
            {"/analysis", analysis_handler, []},
            {"/models", models_handler, []},
            {"/history", history_handler, []},
            {"/harvester", harvester_handler, []},
            {"/settings", settings_handler, []},
            {"/api/update/[...]", update_handler, []},
            {"/api/[...]", api_proxy_handler, []},
            {"/static/[...]", cowboy_static, {priv_dir, desktop_ui, "static"}},
            {"/[...]", not_found_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(http_listener, [{port, Port}],
        #{env => #{dispatch => Dispatch}}),
    
    io:format("Desktop UI started on port ~p~n", [Port]),
    io:format("Open http://localhost:~p in your browser~n", [Port]),
    desktop_ui_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http_listener).
