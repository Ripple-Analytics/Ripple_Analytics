-module(improvement_scheduler).
%% Helper modules: improvement_scheduler_part2
-behaviour(gen_server).

-export([start_link/0, start_cycle/0, stop_cycle/0, get_status/0, force_improvement/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    running :: boolean(),
    timer_ref :: reference() | undefined,
    current_cycle :: integer(),
    last_improvement :: integer(),
    improvements_made :: integer(),
    errors :: list()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_cycle() ->
    gen_server:call(?MODULE, start_cycle).

stop_cycle() ->
    gen_server:call(?MODULE, stop_cycle).

get_status() ->
    gen_server:call(?MODULE, get_status).

force_improvement() ->
    gen_server:call(?MODULE, force_improvement, 300000).

init([]) ->
    AutoStart = application:get_env(ai_code_improver, auto_start, false),
    State = #state{
        running = false,
        timer_ref = undefined,
        current_cycle = 0,
        last_improvement = 0,
        improvements_made = 0,
        errors = []
    },
    case AutoStart of
        true -> 
            self() ! start_auto,
            {ok, State};
        false -> 
            {ok, State}
    end.

handle_call(start_cycle, _From, State) ->
    case State#state.running of
        true ->
            {reply, {error, already_running}, State};
        false ->
            Interval = application:get_env(ai_code_improver, improvement_interval, 300000),
            TimerRef = erlang:send_after(Interval, self(), run_improvement_cycle),
            io:format("[AI Improver] Started improvement cycle (interval: ~p ms)~n", [Interval]),
            {reply, ok, State#state{running = true, timer_ref = TimerRef}}
    end;

handle_call(stop_cycle, _From, State) ->
    case State#state.timer_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    io:format("[AI Improver] Stopped improvement cycle~n"),
    {reply, ok, State#state{running = false, timer_ref = undefined}};

handle_call(get_status, _From, State) ->
    Status = #{
        <<"running">> => State#state.running,
        <<"current_cycle">> => State#state.current_cycle,
        <<"last_improvement">> => State#state.last_improvement,
        <<"improvements_made">> => State#state.improvements_made,
        <<"recent_errors">> => lists:sublist(State#state.errors, 10)
    },
    {reply, {ok, Status}, State};

handle_call(force_improvement, _From, State) ->
    io:format("[AI Improver] Forcing improvement cycle~n"),
    Result = run_improvement(),
    NewState = case Result of
        {ok, Improvements} ->
            improvement_history:record(Improvements),
            State#state{
                last_improvement = erlang:system_time(second),
                improvements_made = State#state.improvements_made + length(Improvements)
            };
        {error, Reason} ->
            State#state{
                errors = [{erlang:system_time(second), Reason} | State#state.errors]
            }
    end,
    {reply, Result, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start_auto, State) ->
    Interval = application:get_env(ai_code_improver, improvement_interval, 300000),
    TimerRef = erlang:send_after(Interval, self(), run_improvement_cycle),
    io:format("[AI Improver] Auto-started improvement cycle~n"),
    {noreply, State#state{running = true, timer_ref = TimerRef}};

handle_info(run_improvement_cycle, State) ->
    io:format("[AI Improver] Running improvement cycle #~p~n", [State#state.current_cycle + 1]),
    
    NewState = case run_improvement() of
        {ok, Improvements} ->
            improvement_history:record(Improvements),
            io:format("[AI Improver] Cycle complete: ~p improvements~n", [length(Improvements)]),
            State#state{
                current_cycle = State#state.current_cycle + 1,
                last_improvement = erlang:system_time(second),
                improvements_made = State#state.improvements_made + length(Improvements)
            };
        {error, Reason} ->
            io:format("[AI Improver] Cycle failed: ~p~n", [Reason]),
            State#state{
                current_cycle = State#state.current_cycle + 1,
                errors = [{erlang:system_time(second), Reason} | State#state.errors]
            }
    end,
    
    case NewState#state.running of
        true ->
            Interval = application:get_env(ai_code_improver, improvement_interval, 300000),
            TimerRef = erlang:send_after(Interval, self(), run_improvement_cycle),
            {noreply, NewState#state{timer_ref = TimerRef}};
        false ->
            {noreply, NewState#state{timer_ref = undefined}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.timer_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ok.

run_improvement() ->
    case lm_studio_client:check_connection() of
        {ok, connected} ->
            run_improvement_pipeline();
        {error, Reason} ->
            {error, {lm_studio_unavailable, Reason}}
    end.
