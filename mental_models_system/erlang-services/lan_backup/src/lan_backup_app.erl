-module(lan_backup_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", lan_health_handler, []},
            {"/api/lan/status", lan_status_handler, []},
            {"/api/lan/config", lan_config_handler, []},
            {"/api/lan/discover", lan_discover_handler, []},
            {"/api/lan/peers", lan_peers_handler, []},
            {"/api/lan/sync", lan_sync_handler, []},
            {"/api/lan/broadcast", lan_broadcast_handler, []},
            {'_', lan_not_found_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(lan_listener,
        [{port, 8010}],
        #{env => #{dispatch => Dispatch}}
    ),
    io:format("LAN/Local Network Backup Service started on port 8010~n"),
    lan_backup_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(lan_listener),
    ok.
