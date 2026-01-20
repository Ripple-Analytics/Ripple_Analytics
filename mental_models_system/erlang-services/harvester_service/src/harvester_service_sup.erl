%%%-------------------------------------------------------------------
%%% @doc Harvester Service Supervisor
%%%-------------------------------------------------------------------
-module(harvester_service_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 60},
    
    ChildSpecs = [
        #{id => scraper_pool,
          start => {scraper_pool, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [scraper_pool]},
        #{id => stats_collector,
          start => {stats_collector, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [stats_collector]}
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
