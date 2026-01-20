%%%-------------------------------------------------------------------
%%% @doc Chaos Monkey Supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(chaos_monkey_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    Children = [
        #{
            id => chaos_engine,
            start => {chaos_engine, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [chaos_engine]
        },
        #{
            id => attack_scheduler,
            start => {attack_scheduler, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [attack_scheduler]
        }
    ],
    
    {ok, {SupFlags, Children}}.
