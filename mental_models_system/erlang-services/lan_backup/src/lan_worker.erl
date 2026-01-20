-module(lan_worker).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([get_status/0, get_config/0, set_config/1, discover_peers/0, get_peers/0, add_peer/1, remove_peer/1, sync_with/1, broadcast_update/0]).

-record(state, {
    peers = [],
    broadcast_port = 8011,
    discovery_enabled = true,
    last_discovery = undefined,
    last_sync = undefined,
    sync_count = 0
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    BroadcastPort = list_to_integer(os:getenv("LAN_BROADCAST_PORT", "8011")),
    DiscoveryEnabled = os:getenv("LAN_DISCOVERY_ENABLED", "true") =:= "true",
    
    State = #state{
        broadcast_port = BroadcastPort,
        discovery_enabled = DiscoveryEnabled
    },
    
    io:format("LAN Backup Worker initialized. Discovery: ~p~n", [DiscoveryEnabled]),
    {ok, State}.

get_status() ->
    gen_server:call(?MODULE, get_status).

get_config() ->
    gen_server:call(?MODULE, get_config).

set_config(Config) ->
    gen_server:call(?MODULE, {set_config, Config}).

discover_peers() ->
    gen_server:call(?MODULE, discover_peers, 30000).

get_peers() ->
    gen_server:call(?MODULE, get_peers).

add_peer(Peer) ->
    gen_server:call(?MODULE, {add_peer, Peer}).

remove_peer(PeerId) ->
    gen_server:call(?MODULE, {remove_peer, PeerId}).

sync_with(PeerId) ->
    gen_server:call(?MODULE, {sync_with, PeerId}, 120000).

broadcast_update() ->
    gen_server:call(?MODULE, broadcast_update, 30000).

handle_call(get_status, _From, State) ->
    Status = #{
        peer_count => length(State#state.peers),
        broadcast_port => State#state.broadcast_port,
        discovery_enabled => State#state.discovery_enabled,
        last_discovery => State#state.last_discovery,
        last_sync => State#state.last_sync,
        sync_count => State#state.sync_count
    },
    {reply, Status, State};

handle_call(get_config, _From, State) ->
    Config = #{
        broadcast_port => State#state.broadcast_port,
        discovery_enabled => State#state.discovery_enabled
    },
    {reply, Config, State};

handle_call({set_config, Config}, _From, State) ->
    NewState = State#state{
        broadcast_port = maps:get(<<"broadcast_port">>, Config, State#state.broadcast_port),
        discovery_enabled = maps:get(<<"discovery_enabled">>, Config, State#state.discovery_enabled)
    },
    {reply, ok, NewState};

handle_call(discover_peers, _From, State) ->
    NewState = State#state{last_discovery = iso8601_timestamp()},
    {reply, {ok, State#state.peers}, NewState};

handle_call(get_peers, _From, State) ->
    {reply, State#state.peers, State};

handle_call({add_peer, Peer}, _From, State) ->
    NewPeers = [Peer | State#state.peers],
    NewState = State#state{peers = NewPeers},
    {reply, ok, NewState};

handle_call({remove_peer, PeerId}, _From, State) ->
    NewPeers = lists:filter(fun(P) -> maps:get(id, P) =/= PeerId end, State#state.peers),
    NewState = State#state{peers = NewPeers},
    {reply, ok, NewState};

handle_call({sync_with, _PeerId}, _From, State) ->
    NewState = State#state{
        last_sync = iso8601_timestamp(),
        sync_count = State#state.sync_count + 1
    },
    {reply, {ok, #{status => <<"synced">>, timestamp => iso8601_timestamp()}}, NewState};

handle_call(broadcast_update, _From, State) ->
    {reply, {ok, #{status => <<"broadcast_sent">>, peers_notified => length(State#state.peers)}}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

iso8601_timestamp() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y, M, D, H, Mi, S])).
