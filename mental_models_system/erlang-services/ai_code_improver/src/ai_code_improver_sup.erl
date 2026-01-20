-module(ai_code_improver_sup).
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
    ChildSpecs = [
        #{
            id => improvement_scheduler,
            start => {improvement_scheduler, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [improvement_scheduler]
        },
        #{
            id => lm_studio_client,
            start => {lm_studio_client, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [lm_studio_client]
        },
        #{
            id => code_analyzer,
            start => {code_analyzer, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [code_analyzer]
        },
        #{
            id => improvement_history,
            start => {improvement_history, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [improvement_history]
        },
        #{
            id => git_integration,
            start => {git_integration, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [git_integration]
        },
        #{
            id => improvement_metrics,
            start => {improvement_metrics, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [improvement_metrics]
        },
        #{
            id => diff_preview,
            start => {diff_preview, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [diff_preview]
        },
        #{
            id => rollback_manager,
            start => {rollback_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [rollback_manager]
        },
        #{
            id => exclusion_config,
            start => {exclusion_config, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [exclusion_config]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
