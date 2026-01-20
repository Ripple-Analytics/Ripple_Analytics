-module(peer_coordinator).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    my_env,
    peer_url,
    is_active = false
}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    MyEnv = os:getenv("DEPLOYMENT_ENV", "blue"),
    PeerEnv = case MyEnv of "blue" -> "green"; _ -> "blue" end,
    PeerUrl = "http://deployment-controller-" ++ PeerEnv ++ ":8007",
    io:format("[PEER-COORD] I am ~s, peer is ~s~n", [MyEnv, PeerEnv]),
    erlang:send_after(30000, self(), check_peer),
    {ok, #state{my_env = MyEnv, peer_url = PeerUrl}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(check_peer, State) ->
    erlang:send_after(60000, self(), check_peer),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.
