%%%-------------------------------------------------------------------
%%% @doc Attack Scheduler
%%% 
%%% Schedules random attacks at configurable intervals.
%%% @end
%%%-------------------------------------------------------------------
-module(attack_scheduler).
-behaviour(gen_server).

-export([start_link/0, set_interval/1, get_schedule/0, trigger_random_attack/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_INTERVAL, 300000). %% 5 minutes in milliseconds

-record(state, {
    interval :: pos_integer(),
    timer_ref :: undefined | reference(),
    next_attack :: undefined | calendar:datetime(),
    attacks_scheduled :: non_neg_integer()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

set_interval(Seconds) when Seconds >= 60 ->
    gen_server:call(?SERVER, {set_interval, Seconds * 1000}).

get_schedule() ->
    gen_server:call(?SERVER, get_schedule).

trigger_random_attack() ->
    gen_server:cast(?SERVER, trigger_random_attack).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Interval = application:get_env(chaos_monkey, attack_interval, 300) * 1000,
    
    %% Schedule first attack check (delayed start to let services come up)
    TimerRef = erlang:send_after(60000, self(), maybe_attack),
    
    {ok, #state{
        interval = Interval,
        timer_ref = TimerRef,
        next_attack = add_milliseconds(calendar:local_time(), 60000),
        attacks_scheduled = 0
    }}.

handle_call({set_interval, NewInterval}, _From, State) ->
    %% Cancel existing timer
    case State#state.timer_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    
    %% Set new timer
    NewTimerRef = erlang:send_after(NewInterval, self(), maybe_attack),
    NewState = State#state{
        interval = NewInterval,
        timer_ref = NewTimerRef,
        next_attack = add_milliseconds(calendar:local_time(), NewInterval)
    },
    
    {reply, ok, NewState};

handle_call(get_schedule, _From, State) ->
    Schedule = #{
        <<"interval_seconds">> => State#state.interval div 1000,
        <<"next_attack">> => format_datetime(State#state.next_attack),
        <<"attacks_scheduled">> => State#state.attacks_scheduled
    },
    {reply, {ok, Schedule}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(trigger_random_attack, State) ->
    %% Execute a random attack immediately
    chaos_engine:execute_attack(random),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(maybe_attack, State) ->
    %% Check if chaos is enabled before attacking
    case chaos_engine:get_status() of
        {ok, #{<<"enabled">> := true}} ->
            io:format("[Attack Scheduler] Executing scheduled random attack~n"),
            chaos_engine:execute_attack(random);
        _ ->
            io:format("[Attack Scheduler] Chaos disabled, skipping attack~n")
    end,
    
    %% Schedule next attack
    NewTimerRef = erlang:send_after(State#state.interval, self(), maybe_attack),
    
    NewState = State#state{
        timer_ref = NewTimerRef,
        next_attack = add_milliseconds(calendar:local_time(), State#state.interval),
        attacks_scheduled = State#state.attacks_scheduled + 1
    },
    
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.timer_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

add_milliseconds(DateTime, Ms) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
    NewSeconds = Seconds + (Ms div 1000),
    calendar:gregorian_seconds_to_datetime(NewSeconds).

format_datetime(undefined) -> <<"never">>;
format_datetime({{Y, M, D}, {H, Mi, S}}) ->
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B", [Y, M, D, H, Mi, S])).
