%%%-------------------------------------------------------------------
%%% @doc Log Checker Service Supervisor
%%% Manages log scanner, error aggregator, and fix pipeline workers.
%%% @end
%%%-------------------------------------------------------------------
-module(log_checker_service_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    Children = [
        %% Log Scanner - continuously monitors docker logs
        #{id => log_scanner,
          start => {log_scanner, start_link, []},
          restart => permanent,
          type => worker},
        
        %% Error Aggregator - collects and deduplicates errors
        #{id => error_aggregator,
          start => {error_aggregator, start_link, []},
          restart => permanent,
          type => worker},
        
        %% Fix Pipeline - sends errors to LM Studio and applies fixes
        #{id => fix_pipeline,
          start => {fix_pipeline, start_link, []},
          restart => permanent,
          type => worker},
        
        %% Build Monitor - watches docker build output
        #{id => build_monitor,
          start => {build_monitor, start_link, []},
          restart => permanent,
          type => worker}
    ],
    
    {ok, {SupFlags, Children}}.
