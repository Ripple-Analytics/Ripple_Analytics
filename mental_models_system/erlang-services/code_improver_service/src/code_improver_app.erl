-module(code_improver_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("[CODE_IMPROVER] ========================================~n"),
    io:format("[CODE_IMPROVER] Autonomous Code Improvement Service~n"),
    io:format("[CODE_IMPROVER] Munger Philosophy Aligned~n"),
    io:format("[CODE_IMPROVER] ========================================~n"),
    
    Port = application:get_env(code_improver_service, port, 8010),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/status", improver_status_handler, []},
            {"/trigger", improver_trigger_handler, []},
            {"/improvements", improver_status_handler, [list]},
            {"/improvements/:id", improver_status_handler, [detail]}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("[CODE_IMPROVER] HTTP server started on port ~p~n", [Port]),
    code_improver_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http_listener),
    ok.
