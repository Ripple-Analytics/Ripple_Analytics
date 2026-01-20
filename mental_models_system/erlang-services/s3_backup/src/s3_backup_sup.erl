-module(s3_backup_sup).
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
    ChildSpecs = [
        #{
            id => s3_worker,
            start => {s3_worker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [s3_worker]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
