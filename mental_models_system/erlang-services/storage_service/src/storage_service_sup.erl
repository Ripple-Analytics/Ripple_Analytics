%%%-------------------------------------------------------------------
%%% @doc Storage Service Supervisor
%%%-------------------------------------------------------------------
-module(storage_service_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 60},
    ChildSpecs = [
        #{id => document_store,
          start => {document_store, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker},
        #{id => history_store,
          start => {history_store, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker}
    ],
    {ok, {SupFlags, ChildSpecs}}.
