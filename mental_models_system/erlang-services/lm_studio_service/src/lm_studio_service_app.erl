%%%-------------------------------------------------------------------
%%% @doc LM Studio Service Application
%%% 
%%% Blue-green deployable LLM service for autonomous code improvements
%%% and self-healing deployments. Connects to LM Studio running on
%%% the host machine.
%%%
%%% Features:
%%% - Auto-fix compilation errors
%%% - Self-healing deployment monitoring
%%% - Code improvement suggestions
%%% - Build failure analysis and repair
%%% @end
%%%-------------------------------------------------------------------
-module(lm_studio_service_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("[LM-STUDIO] ========================================~n"),
    io:format("[LM-STUDIO] LM STUDIO SERVICE STARTING~n"),
    io:format("[LM-STUDIO] Autonomous code improvement enabled~n"),
    io:format("[LM-STUDIO] ========================================~n"),
    
    %% Get port from environment or default to 8007
    Port = get_port(),
    
    %% Setup routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/api/lm/status", lm_status_handler, []},
            {"/api/lm/generate", lm_generate_handler, []},
            {"/api/lm/fix-error", lm_fix_error_handler, []},
            {"/api/lm/fix-build", lm_fix_build_handler, []},
            {"/api/lm/improve", lm_improve_handler, []},
            {"/api/lm/analyze", lm_analyze_handler, []},
            {"/api/lm/self-heal", lm_self_heal_handler, []},
            {"/[...]", not_found_handler, []}
        ]}
    ]),
    
    %% Start Cowboy
    {ok, _} = cowboy:start_clear(lm_studio_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("[LM-STUDIO] HTTP server listening on port ~p~n", [Port]),
    
    %% Start the supervisor
    lm_studio_service_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(lm_studio_http_listener),
    ok.

get_port() ->
    case os:getenv("LM_STUDIO_SERVICE_PORT") of
        false -> 8007;
        PortStr ->
            try list_to_integer(PortStr)
            catch _:_ -> 8007
            end
    end.
