-module(improvement_metrics).
-behaviour(gen_server).

-export([start_link/0, record_improvement/1, record_failure/1, get_metrics/0]).
-export([get_success_rate/0, get_improvements_by_type/0, reset_metrics/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    total_improvements :: non_neg_integer(),
    total_failures :: non_neg_integer(),
    improvements_by_type :: map(),
    improvements_by_file :: map(),
    cycle_times :: list(),
    last_improvement :: undefined | erlang:timestamp(),
    started_at :: erlang:timestamp()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

record_improvement(Improvement) ->
    gen_server:cast(?MODULE, {record_improvement, Improvement}).

record_failure(Reason) ->
    gen_server:cast(?MODULE, {record_failure, Reason}).

get_metrics() ->
    gen_server:call(?MODULE, get_metrics, 5000).

get_success_rate() ->
    gen_server:call(?MODULE, get_success_rate, 5000).

get_improvements_by_type() ->
    gen_server:call(?MODULE, get_improvements_by_type, 5000).

reset_metrics() ->
    gen_server:call(?MODULE, reset_metrics, 5000).

init([]) ->
    {ok, #state{
        total_improvements = 0,
        total_failures = 0,
        improvements_by_type = #{},
        improvements_by_file = #{},
        cycle_times = [],
        last_improvement = undefined,
        started_at = erlang:timestamp()
    }}.

handle_call(get_metrics, _From, State) ->
    Uptime = timer:now_diff(erlang:timestamp(), State#state.started_at) div 1000000,
    AvgCycleTime = case State#state.cycle_times of
        [] -> 0;
        Times -> lists:sum(Times) div length(Times)
    end,
    Metrics = #{
        <<"total_improvements">> => State#state.total_improvements,
        <<"total_failures">> => State#state.total_failures,
        <<"success_rate">> => calculate_success_rate(State),
        <<"improvements_by_type">> => State#state.improvements_by_type,
        <<"improvements_by_file">> => maps:fold(fun(K, V, Acc) ->
            maps:put(list_to_binary(K), V, Acc)
        end, #{}, State#state.improvements_by_file),
        <<"avg_cycle_time_ms">> => AvgCycleTime,
        <<"uptime_seconds">> => Uptime,
        <<"last_improvement">> => format_timestamp(State#state.last_improvement)
    },
    {reply, {ok, Metrics}, State};

handle_call(get_success_rate, _From, State) ->
    Rate = calculate_success_rate(State),
    {reply, {ok, Rate}, State};

handle_call(get_improvements_by_type, _From, State) ->
    {reply, {ok, State#state.improvements_by_type}, State};

handle_call(reset_metrics, _From, _State) ->
    NewState = #state{
        total_improvements = 0,
        total_failures = 0,
        improvements_by_type = #{},
        improvements_by_file = #{},
        cycle_times = [],
        last_improvement = undefined,
        started_at = erlang:timestamp()
    },
    {reply, ok, NewState}.

handle_cast({record_improvement, Improvement}, State) ->
    Type = maps:get(<<"type">>, Improvement, <<"unknown">>),
    File = maps:get(<<"file">>, Improvement, "unknown"),
    CycleTime = maps:get(<<"cycle_time_ms">>, Improvement, 0),
    
    NewByType = maps:update_with(Type, fun(V) -> V + 1 end, 1, State#state.improvements_by_type),
    NewByFile = maps:update_with(File, fun(V) -> V + 1 end, 1, State#state.improvements_by_file),
    NewCycleTimes = lists:sublist([CycleTime | State#state.cycle_times], 100),
    
    NewState = State#state{
        total_improvements = State#state.total_improvements + 1,
        improvements_by_type = NewByType,
        improvements_by_file = NewByFile,
        cycle_times = NewCycleTimes,
        last_improvement = erlang:timestamp()
    },
    {noreply, NewState};

handle_cast({record_failure, _Reason}, State) ->
    NewState = State#state{
        total_failures = State#state.total_failures + 1
    },
    {noreply, NewState}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

calculate_success_rate(State) ->
    Total = State#state.total_improvements + State#state.total_failures,
    case Total of
        0 -> 100.0;
        _ -> (State#state.total_improvements / Total) * 100
    end.

format_timestamp(undefined) -> null;
format_timestamp(Timestamp) ->
    {{Y, M, D}, {H, Mi, S}} = calendar:now_to_datetime(Timestamp),
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", 
                                  [Y, M, D, H, Mi, S])).
