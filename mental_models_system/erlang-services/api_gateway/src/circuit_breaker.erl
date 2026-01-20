%%%-------------------------------------------------------------------
%%% @doc Circuit Breaker
%%% 
%%% Implements the circuit breaker pattern for fault tolerance.
%%% States: closed (normal), open (fail-fast), half_open (testing)
%%% @end
%%%-------------------------------------------------------------------
-module(circuit_breaker).
-behaviour(gen_server).

-export([start_link/0, call/2, get_state/1, reset/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(FAILURE_THRESHOLD, 5).
-define(RESET_TIMEOUT, 60000). %% 60 seconds
-define(HALF_OPEN_SUCCESS_THRESHOLD, 3).

-record(breaker_state, {
    state = closed :: closed | open | half_open,
    failure_count = 0 :: non_neg_integer(),
    success_count = 0 :: non_neg_integer(),
    last_failure_time = undefined :: undefined | integer()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

call(Service, Fun) ->
    gen_server:call(?SERVER, {call, Service, Fun}, 60000).

get_state(Service) ->
    gen_server:call(?SERVER, {get_state, Service}).

reset(Service) ->
    gen_server:cast(?SERVER, {reset, Service}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Use ETS for fast state lookups
    ets:new(circuit_breakers, [named_table, public, {keypos, 1}]),
    {ok, #{}}.

handle_call({call, Service, Fun}, _From, State) ->
    BreakerState = get_breaker_state(Service),
    
    case BreakerState#breaker_state.state of
        open ->
            %% Check if reset timeout has passed
            Now = erlang:system_time(millisecond),
            LastFailure = BreakerState#breaker_state.last_failure_time,
            
            if
                LastFailure =:= undefined orelse (Now - LastFailure) > ?RESET_TIMEOUT ->
                    %% Transition to half-open
                    NewState = BreakerState#breaker_state{state = half_open, success_count = 0},
                    set_breaker_state(Service, NewState),
                    execute_with_tracking(Service, Fun, NewState);
                true ->
                    {reply, {error, circuit_open}, State}
            end;
        
        half_open ->
            execute_with_tracking(Service, Fun, BreakerState);
        
        closed ->
            execute_with_tracking(Service, Fun, BreakerState)
    end;

handle_call({get_state, Service}, _From, State) ->
    BreakerState = get_breaker_state(Service),
    {reply, BreakerState#breaker_state.state, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({reset, Service}, State) ->
    set_breaker_state(Service, #breaker_state{}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

get_breaker_state(Service) ->
    case ets:lookup(circuit_breakers, Service) of
        [{Service, BreakerState}] -> BreakerState;
        [] -> #breaker_state{}
    end.

set_breaker_state(Service, BreakerState) ->
    ets:insert(circuit_breakers, {Service, BreakerState}).

execute_with_tracking(Service, Fun, BreakerState) ->
    try
        Result = Fun(),
        handle_success(Service, BreakerState),
        {reply, Result, #{}}
    catch
        _:Reason ->
            handle_failure(Service, BreakerState),
            {reply, {error, Reason}, #{}}
    end.

handle_success(Service, BreakerState) ->
    case BreakerState#breaker_state.state of
        half_open ->
            NewSuccessCount = BreakerState#breaker_state.success_count + 1,
            if
                NewSuccessCount >= ?HALF_OPEN_SUCCESS_THRESHOLD ->
                    %% Transition back to closed
                    set_breaker_state(Service, #breaker_state{});
                true ->
                    NewState = BreakerState#breaker_state{success_count = NewSuccessCount},
                    set_breaker_state(Service, NewState)
            end;
        _ ->
            %% Reset failure count on success
            set_breaker_state(Service, #breaker_state{})
    end.

handle_failure(Service, BreakerState) ->
    NewFailureCount = BreakerState#breaker_state.failure_count + 1,
    Now = erlang:system_time(millisecond),
    
    if
        NewFailureCount >= ?FAILURE_THRESHOLD ->
            %% Open the circuit
            NewState = BreakerState#breaker_state{
                state = open,
                failure_count = NewFailureCount,
                last_failure_time = Now
            },
            set_breaker_state(Service, NewState);
        true ->
            NewState = BreakerState#breaker_state{
                failure_count = NewFailureCount,
                last_failure_time = Now
            },
            set_breaker_state(Service, NewState)
    end.
