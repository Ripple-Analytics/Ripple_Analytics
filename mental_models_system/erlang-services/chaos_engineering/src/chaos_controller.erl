%%%-------------------------------------------------------------------
%%% @doc Chaos Controller - Manages fault injection and chaos experiments
%%%-------------------------------------------------------------------
-module(chaos_controller).
-behaviour(gen_server).

-export([start_link/0, inject_latency/2, inject_error/2, inject_failure/1,
         get_service_health/0, clear_injections/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVICES, [
    {api_gateway, "http://api-gateway:8000"},
    {analysis, "http://analysis-service:8001"},
    {harvester, "http://harvester-service:8002"},
    {storage, "http://storage-service:8003"}
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

inject_latency(Service, DelayMs) ->
    gen_server:call(?MODULE, {inject_latency, Service, DelayMs}).

inject_error(Service, ErrorRate) ->
    gen_server:call(?MODULE, {inject_error, Service, ErrorRate}).

inject_failure(Service) ->
    gen_server:call(?MODULE, {inject_failure, Service}).

get_service_health() ->
    gen_server:call(?MODULE, get_health, 30000).

clear_injections() ->
    gen_server:call(?MODULE, clear).

init([]) ->
    {ok, #{injections => #{}, health_cache => #{}}}.

handle_call({inject_latency, Service, DelayMs}, _From, #{injections := Inj} = State) ->
    NewInj = maps:put({latency, Service}, DelayMs, Inj),
    {reply, {ok, #{service => Service, latency_ms => DelayMs}}, State#{injections := NewInj}};

handle_call({inject_error, Service, ErrorRate}, _From, #{injections := Inj} = State) ->
    NewInj = maps:put({error, Service}, ErrorRate, Inj),
    {reply, {ok, #{service => Service, error_rate => ErrorRate}}, State#{injections := NewInj}};

handle_call({inject_failure, Service}, _From, #{injections := Inj} = State) ->
    NewInj = maps:put({failure, Service}, true, Inj),
    {reply, {ok, #{service => Service, status => failed}}, State#{injections := NewInj}};

handle_call(get_health, _From, State) ->
    Health = lists:map(fun({Name, Url}) ->
        Status = check_health(Url),
        #{name => Name, url => list_to_binary(Url), status => Status}
    end, ?SERVICES),
    {reply, Health, State};

handle_call(clear, _From, State) ->
    {reply, ok, State#{injections := #{}}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.

check_health(BaseUrl) ->
    Url = BaseUrl ++ "/health",
    case hackney:request(get, list_to_binary(Url), [], <<>>, [{timeout, 5000}]) of
        {ok, 200, _, _} -> <<"healthy">>;
        {ok, Status, _, _} -> list_to_binary(io_lib:format("unhealthy (~p)", [Status]));
        {error, Reason} -> list_to_binary(io_lib:format("error: ~p", [Reason]))
    end.
