%%%-------------------------------------------------------------------
%%% @doc API Gateway Supervisor
%%% 
%%% Top-level supervisor for the API Gateway.
%%% Implements OTP supervisor behavior for fault tolerance.
%%% @end
%%%-------------------------------------------------------------------
-module(api_gateway_sup).
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
%% 
%% Supervisor strategy: one_for_one
%% - If a child process terminates, only that process is restarted
%% - Max 10 restarts in 60 seconds before supervisor gives up
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    %% Child specifications
    ChildSpecs = [
        %% Circuit breaker manager
        #{
            id => circuit_breaker,
            start => {circuit_breaker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [circuit_breaker]
        },
        %% Service registry
        #{
            id => service_registry,
            start => {service_registry, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [service_registry]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
