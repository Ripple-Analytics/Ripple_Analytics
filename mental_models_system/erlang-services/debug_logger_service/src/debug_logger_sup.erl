%%%-------------------------------------------------------------------
%%% @doc Debug Logger Supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(debug_logger_sup).
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
            id => debug_logger_worker,
            start => {debug_logger_worker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [debug_logger_worker]
        }
    ],
    
    {ok, {SupFlags, Children}}.
