%%%-------------------------------------------------------------------
%%% @doc Self Healer - Monitors services and auto-repairs failures
%%% @end
%%%-------------------------------------------------------------------
-module(self_healer).
-behaviour(gen_server).

-export([start_link/0, get_status/0, check_all_services/0, heal_service/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    timer_ref :: reference() | undefined,
    services = [] :: list(),
    unhealthy_services = [] :: list(),
    heal_history = [] :: list(),
    total_heals = 0 :: integer(),
    successful_heals = 0 :: integer()
}).

-define(CHECK_INTERVAL, 60000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_status() ->
    gen_server:call(?MODULE, get_status).

check_all_services() ->
    gen_server:call(?MODULE, check_all, 30000).

heal_service(ServiceName) ->
    gen_server:call(?MODULE, {heal, ServiceName}, 120000).

init([]) ->
    io:format("[SELF-HEALER] Self-healer initialized~n"),
    Services = [
        #{name => <<"desktop-ui">>, port => 3000},
        #{name => <<"analysis-service">>, port => 8001},
        #{name => <<"storage-service">>, port => 8002}
    ],
    TimerRef = erlang:send_after(?CHECK_INTERVAL, self(), check_services),
    {ok, #state{timer_ref = TimerRef, services = Services}}.

handle_call(get_status, _From, State) ->
    Status = #{
        <<"unhealthy">> => State#state.unhealthy_services,
        <<"total_heals">> => State#state.total_heals,
        <<"successful_heals">> => State#state.successful_heals
    },
    {reply, {ok, Status}, State};

handle_call(check_all, _From, State) ->
    Unhealthy = check_services_health(State#state.services),
    {reply, {ok, Unhealthy}, State#state{unhealthy_services = Unhealthy}};

handle_call({heal, ServiceName}, _From, State) ->
    Result = do_heal_service(ServiceName),
    NewState = record_heal(State, ServiceName, Result),
    {reply, Result, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_services, State) ->
    TimerRef = erlang:send_after(?CHECK_INTERVAL, self(), check_services),
    Unhealthy = check_services_health(State#state.services),
    NewState = lists:foldl(fun(S, Acc) ->
        Name = maps:get(name, S),
        Result = do_heal_service(Name),
        record_heal(Acc, Name, Result)
    end, State#state{unhealthy_services = Unhealthy}, Unhealthy),
    {noreply, NewState#state{timer_ref = TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

check_services_health(Services) ->
    [S || S <- Services, not is_healthy(S)].

is_healthy(#{name := Name, port := Port}) ->
    Url = iolist_to_binary(["http://localhost:", integer_to_binary(Port), "/health"]),
    case hackney:request(get, Url, [], <<>>, [{timeout, 5000}]) of
        {ok, 200, _, Ref} -> hackney:body(Ref), true;
        _ -> false
    end.

do_heal_service(ServiceName) ->
    Container = "mental-models-" ++ binary_to_list(ServiceName),
    os:cmd("docker restart " ++ Container),
    timer:sleep(5000),
    {ok, restarted}.

record_heal(State, _Name, {ok, _}) ->
    State#state{
        total_heals = State#state.total_heals + 1,
        successful_heals = State#state.successful_heals + 1
    };
record_heal(State, _Name, _) ->
    State#state{total_heals = State#state.total_heals + 1}.
