%%%-------------------------------------------------------------------
%%% @doc Analysis Service Supervisor
%%% 
%%% Top-level supervisor for the Analysis Service.
%%% Manages model registry and LLM client workers.
%%% @end
%%%-------------------------------------------------------------------
-module(analysis_service_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @doc Start the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc Initialize the supervisor
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    ChildSpecs = [
        %% Model registry - stores mental models
        #{
            id => model_registry,
            start => {model_registry, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [model_registry]
        },
        %% LLM client - handles LM Studio integration
        #{
            id => llm_client,
            start => {llm_client, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [llm_client]
        },
        %% Folder watcher - monitors folders for automatic analysis
        #{
            id => folder_watcher,
            start => {folder_watcher, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [folder_watcher]
        },
        %% Notification service - real-time analysis alerts
        #{
            id => notification_service,
            start => {notification_service, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [notification_service]
        },
        %% Analytics service - aggregated statistics and trends
        #{
            id => analytics_service,
            start => {analytics_service, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [analytics_service]
        },
        %% Code improver service - autonomous code improvement with LM Studio
        #{
            id => code_improver_service,
            start => {code_improver_service, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [code_improver_service]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
