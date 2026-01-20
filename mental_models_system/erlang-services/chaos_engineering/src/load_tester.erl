%%%-------------------------------------------------------------------
%%% @doc Load Tester - Concurrent load testing with metrics
%%%-------------------------------------------------------------------
-module(load_tester).
-behaviour(gen_server).

-export([start_link/0, run_test/3, get_results/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVICES, #{
    <<"api_gateway">> => "http://api-gateway:8000/health",
    <<"analysis">> => "http://analysis-service:8001/health",
    <<"harvester">> => "http://harvester-service:8002/health",
    <<"storage">> => "http://storage-service:8003/health"
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

run_test(Target, Requests, Concurrency) ->
    gen_server:call(?MODULE, {run, Target, Requests, Concurrency}, 120000).

get_results() ->
    gen_server:call(?MODULE, get_results).

init([]) ->
    {ok, #{results => []}}.

handle_call({run, Target, Requests, Concurrency}, _From, State) ->
    Url = maps:get(Target, ?SERVICES, "http://api-gateway:8000/health"),
    Result = execute_load_test(Url, Requests, Concurrency),
    NewResults = [Result | maps:get(results, State)],
    {reply, Result, State#{results := NewResults}};

handle_call(get_results, _From, #{results := Results} = State) ->
    {reply, Results, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.

execute_load_test(Url, TotalRequests, Concurrency) ->
    StartTime = erlang:system_time(millisecond),
    
    %% Spawn workers
    Parent = self(),
    Workers = [spawn_link(fun() ->
        Results = worker_loop(Url, TotalRequests div Concurrency, []),
        Parent ! {worker_done, Results}
    end) || _ <- lists:seq(1, Concurrency)],
    
    %% Collect results
    AllLatencies = collect_results(length(Workers), []),
    
    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,
    
    %% Calculate metrics
    Sorted = lists:sort(AllLatencies),
    Successful = [L || L <- Sorted, L > 0],
    Failed = length(Sorted) - length(Successful),
    
    #{
        <<"target">> => list_to_binary(Url),
        <<"total_requests">> => TotalRequests,
        <<"concurrency">> => Concurrency,
        <<"duration_ms">> => Duration,
        <<"successful">> => length(Successful),
        <<"failed">> => Failed,
        <<"requests_per_second">> => (TotalRequests * 1000) / max(Duration, 1),
        <<"latency">> => #{
            <<"min">> => safe_min(Successful),
            <<"max">> => safe_max(Successful),
            <<"avg">> => safe_avg(Successful),
            <<"p50">> => percentile(Successful, 50),
            <<"p95">> => percentile(Successful, 95),
            <<"p99">> => percentile(Successful, 99)
        }
    }.

worker_loop(_Url, 0, Results) -> Results;
worker_loop(Url, N, Results) ->
    Start = erlang:system_time(millisecond),
    Latency = case hackney:request(get, list_to_binary(Url), [], <<>>, [{timeout, 10000}]) of
        {ok, 200, _, ClientRef} ->
            hackney:body(ClientRef),
            erlang:system_time(millisecond) - Start;
        _ ->
            -1  %% Failed request
    end,
    worker_loop(Url, N - 1, [Latency | Results]).

collect_results(0, Acc) -> lists:flatten(Acc);
collect_results(N, Acc) ->
    receive
        {worker_done, Results} -> collect_results(N - 1, [Results | Acc])
    after 60000 ->
        collect_results(N - 1, Acc)
    end.

safe_min([]) -> 0;
safe_min(List) -> lists:min(List).

safe_max([]) -> 0;
safe_max(List) -> lists:max(List).

safe_avg([]) -> 0;
safe_avg(List) -> lists:sum(List) / length(List).

percentile([], _) -> 0;
percentile(Sorted, P) ->
    Idx = max(1, round(length(Sorted) * P / 100)),
    lists:nth(min(Idx, length(Sorted)), Sorted).
