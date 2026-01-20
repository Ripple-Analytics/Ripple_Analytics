%%%-------------------------------------------------------------------
%%% @doc LM Studio Service Supervisor
%%% 
%%% Supervises all autonomous components:
%%% - lm_client: Connection to LM Studio
%%% - error_fixer: Auto-fixes compilation errors
%%% - self_healer: Monitors and repairs services
%%% - autonomous_engine: Self-directed research and improvement
%%% - ui_customizer: Autonomous UI improvements
%%% @end
%%%-------------------------------------------------------------------
-module(lm_studio_service_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    Children = [
        %% LM Studio client - connects to LM Studio on host
        #{
            id => lm_client,
            start => {lm_client, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [lm_client]
        },
        %% Error fixer - monitors and fixes compilation errors
        #{
            id => error_fixer,
            start => {error_fixer, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [error_fixer]
        },
        %% Self-healer - monitors service health and auto-repairs
        #{
            id => self_healer,
            start => {self_healer, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [self_healer]
        },
        %% Autonomous engine - self-directed research and improvement
        #{
            id => autonomous_engine,
            start => {autonomous_engine, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [autonomous_engine]
        },
        %% UI customizer - autonomous UI improvements
        #{
            id => ui_customizer,
            start => {ui_customizer, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [ui_customizer]
        }
    ],
    
    {ok, {SupFlags, Children}}.
