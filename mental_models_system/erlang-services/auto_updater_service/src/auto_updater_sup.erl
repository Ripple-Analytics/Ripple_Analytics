%%%-------------------------------------------------------------------
%%% @doc Auto-Updater Supervisor
%%% Supervises the update worker with restart strategy for reliability.
%%% @end
%%%-------------------------------------------------------------------
-module(auto_updater_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% AGGRESSIVE restart strategy - the auto-updater must NEVER stop
    %% Allow up to 100 restarts in 60 seconds before giving up
    %% This ensures maximum reliability for the update system
    SupFlags = #{
        strategy => one_for_one,
        intensity => 100,
        period => 60
    },
    
    Children = [
        #{
            id => updater_worker,
            start => {updater_worker, start_link, []},
            restart => permanent,  %% Always restart, no matter what
            shutdown => 10000,     %% Give 10 seconds for graceful shutdown
            type => worker,
            modules => [updater_worker]
        }
    ],
    
    io:format("[AUTO-UPDATER] Supervisor initialized with aggressive restart policy~n"),
    {ok, {SupFlags, Children}}.
