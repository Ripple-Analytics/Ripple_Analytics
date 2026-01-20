%%%-------------------------------------------------------------------
%%% @doc Google Drive Backup Supervisor
%%% Supervises the backup worker with restart strategy for reliability.
%%% @end
%%%-------------------------------------------------------------------
-module(gdrive_backup_sup).
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
        #{
            id => backup_worker,
            start => {backup_worker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [backup_worker]
        }
    ],
    
    {ok, {SupFlags, Children}}.
