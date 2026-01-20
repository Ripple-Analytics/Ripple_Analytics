%%%-------------------------------------------------------------------
%%% @doc Service Registry
%%% 
%%% Manages service discovery and health monitoring for backend services.
%%% @end
%%%-------------------------------------------------------------------
-module(service_registry).
-behaviour(gen_server).

-export([start_link/0, register_service/2, get_service/1, list_services/0, health_check_all/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(HEALTH_CHECK_INTERVAL, 30000). %% 30 seconds

-record(service, {
    name :: atom(),
    url :: string(),
    status :: healthy | unhealthy | unknown,
    last_check :: integer() | undefined
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register_service(Name, Url) ->
    gen_server:call(?SERVER, {register, Name, Url}).

get_service(Name) ->
    gen_server:call(?SERVER, {get, Name}).

list_services() ->
    gen_server:call(?SERVER, list).

health_check_all() ->
    gen_server:cast(?SERVER, health_check_all).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Initialize ETS table for services
    ets:new(services, [named_table, public, {keypos, #service.name}]),
    
    %% Register default services
    DefaultServices = [
        {analysis, application:get_env(api_gateway, analysis_service_url, "http://analysis-service:8001")},
        {harvester, application:get_env(api_gateway, harvester_service_url, "http://harvester-service:8002")},
        {storage, application:get_env(api_gateway, storage_service_url, "http://storage-service:8003")}
    ],
    
    lists:foreach(fun({Name, Url}) ->
        ets:insert(services, #service{name = Name, url = Url, status = unknown})
    end, DefaultServices),
    
    %% Start periodic health checks
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), health_check),
    
    {ok, #{}}.

handle_call({register, Name, Url}, _From, State) ->
    Service = #service{name = Name, url = Url, status = unknown},
    ets:insert(services, Service),
    {reply, ok, State};

handle_call({get, Name}, _From, State) ->
    case ets:lookup(services, Name) of
        [Service] -> {reply, {ok, Service}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call(list, _From, State) ->
    Services = ets:tab2list(services),
    {reply, Services, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(health_check_all, State) ->
    do_health_check_all(),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(health_check, State) ->
    do_health_check_all(),
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), health_check),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

do_health_check_all() ->
    Services = ets:tab2list(services),
    lists:foreach(fun(Service) ->
        spawn(fun() -> check_service_health(Service) end)
    end, Services).

check_service_health(#service{name = Name, url = Url} = Service) ->
    HealthUrl = Url ++ "/health",
    Now = erlang:system_time(millisecond),
    
    Status = case hackney:request(get, list_to_binary(HealthUrl), [], <<>>, [{timeout, 5000}]) of
        {ok, 200, _, _} -> healthy;
        _ -> unhealthy
    end,
    
    UpdatedService = Service#service{status = Status, last_check = Now},
    ets:insert(services, UpdatedService).
