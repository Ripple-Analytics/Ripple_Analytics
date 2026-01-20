%%%-------------------------------------------------------------------
%%% @doc Google Drive Backup Supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(gdrive_backup_sup).
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
            id => gdrive_client,
            start => {gdrive_client, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [gdrive_client]
        },
        #{
            id => backup_scheduler,
            start => {backup_scheduler, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [backup_scheduler]
        }
    ],
    
    {ok, {SupFlags, Children}}.
