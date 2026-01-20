-module(blue_green_orchestrator_sup).
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
        #{
            id => deployment_state,
            start => {deployment_state, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [deployment_state]
        }
    ],
    
    {ok, {SupFlags, Children}}.
