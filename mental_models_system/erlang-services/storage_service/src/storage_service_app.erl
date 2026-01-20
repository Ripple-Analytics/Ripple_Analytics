%%%-------------------------------------------------------------------
%%% @doc Storage Service Application - Data persistence with DETS
%%%-------------------------------------------------------------------
-module(storage_service_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Port = application:get_env(storage_service, port, 8003),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/api/storage/documents", documents_handler, []},
            {"/api/storage/documents/:id", document_handler, []},
            {"/api/storage/search", search_handler, []},
            {"/[...]", not_found_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(http_listener, [{port, Port}],
        #{env => #{dispatch => Dispatch}}),
    
    io:format("Storage Service started on port ~p~n", [Port]),
    storage_service_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http_listener).
