%%%-------------------------------------------------------------------
%%% @doc Analytics Service - Aggregated Statistics and Trends
%%% 
%%% Provides aggregated analytics from all analysis results including:
%%% - Most frequently detected mental models
%%% - Lollapalooza effect frequency
%%% - Analysis trends over time
%%% - Category distribution
%%% @end
%%%-------------------------------------------------------------------
-module(analytics_service).
-behaviour(gen_server).

-export([start_link/0, record_analysis/1, get_stats/0, get_trends/0, reset_stats/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {
    total_analyses = 0 :: non_neg_integer(),
    lollapalooza_count = 0 :: non_neg_integer(),
    model_counts = #{} :: map(),
    category_counts = #{} :: map(),
    daily_counts = #{} :: map(),
    hourly_counts = #{} :: map(),
    recent_analyses = [] :: list(),
    started_at = undefined :: undefined | calendar:datetime()
}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

record_analysis(Result) ->
    gen_server:cast(?SERVER, {record_analysis, Result}).

get_stats() ->
    gen_server:call(?SERVER, get_stats).

get_trends() ->
    gen_server:call(?SERVER, get_trends).

reset_stats() ->
    gen_server:call(?SERVER, reset_stats).

init([]) ->
    io:format("Analytics Service started~n"),
    {ok, #state{started_at = calendar:local_time()}}.

handle_call(get_stats, _From, State) ->
    Stats = #{
        <<"total_analyses">> => State#state.total_analyses,
        <<"lollapalooza_count">> => State#state.lollapalooza_count,
        <<"lollapalooza_rate">> => calculate_rate(State#state.lollapalooza_count, State#state.total_analyses),
        <<"top_models">> => get_top_items(State#state.model_counts, 10),
        <<"category_distribution">> => maps:to_list(State#state.category_counts),
        <<"uptime_hours">> => calculate_uptime(State#state.started_at),
        <<"analyses_per_hour">> => calculate_rate(State#state.total_analyses, max(1, calculate_uptime(State#state.started_at)))
    },
    {reply, {ok, Stats}, State};

handle_call(get_trends, _From, State) ->
    Trends = #{
        <<"daily">> => format_daily_trends(State#state.daily_counts),
        <<"hourly">> => format_hourly_trends(State#state.hourly_counts),
        <<"recent">> => lists:sublist(State#state.recent_analyses, 20)
    },
    {reply, {ok, Trends}, State};

handle_call(reset_stats, _From, _State) ->
    {reply, {ok, #{status => reset}}, #state{started_at = calendar:local_time()}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({record_analysis, Result}, State) ->
    NewState = process_analysis(Result, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

process_analysis(Result, State) ->
    Now = calendar:local_time(),
    {Date, {Hour, _, _}} = Now,
    
    Models = maps:get(<<"models">>, Result, maps:get(<<"top_models">>, Result, [])),
    LollapaloozaDetected = maps:get(<<"lollapalooza_detected">>, Result, false),
    
    NewModelCounts = lists:foldl(fun(Model, Acc) ->
        Name = maps:get(<<"name">>, Model, <<"Unknown">>),
        maps:update_with(Name, fun(V) -> V + 1 end, 1, Acc)
    end, State#state.model_counts, Models),
    
    NewCategoryCounts = lists:foldl(fun(Model, Acc) ->
        Category = maps:get(<<"category">>, Model, <<"Unknown">>),
        maps:update_with(Category, fun(V) -> V + 1 end, 1, Acc)
    end, State#state.category_counts, Models),
    
    DateKey = format_date(Date),
    NewDailyCounts = maps:update_with(DateKey, fun(V) -> V + 1 end, 1, State#state.daily_counts),
    
    HourKey = integer_to_binary(Hour),
    NewHourlyCounts = maps:update_with(HourKey, fun(V) -> V + 1 end, 1, State#state.hourly_counts),
    
    RecentEntry = #{
        <<"timestamp">> => format_datetime(Now),
        <<"models_count">> => length(Models),
        <<"lollapalooza">> => LollapaloozaDetected,
        <<"top_model">> => case Models of
            [M | _] -> maps:get(<<"name">>, M, <<"None">>);
            [] -> <<"None">>
        end
    },
    
    NewRecent = lists:sublist([RecentEntry | State#state.recent_analyses], 100),
    
    State#state{
        total_analyses = State#state.total_analyses + 1,
        lollapalooza_count = State#state.lollapalooza_count + bool_to_int(LollapaloozaDetected),
        model_counts = NewModelCounts,
        category_counts = NewCategoryCounts,
        daily_counts = NewDailyCounts,
        hourly_counts = NewHourlyCounts,
        recent_analyses = NewRecent
    }.

get_top_items(Map, N) ->
    Sorted = lists:sort(fun({_, A}, {_, B}) -> A > B end, maps:to_list(Map)),
    [#{<<"name">> => Name, <<"count">> => Count} || {Name, Count} <- lists:sublist(Sorted, N)].

calculate_rate(_, 0) -> 0.0;
calculate_rate(Num, Denom) -> round(Num / Denom * 100) / 100.

calculate_uptime(undefined) -> 0;
calculate_uptime(StartTime) ->
    Now = calendar:local_time(),
    StartSecs = calendar:datetime_to_gregorian_seconds(StartTime),
    NowSecs = calendar:datetime_to_gregorian_seconds(Now),
    (NowSecs - StartSecs) / 3600.

format_daily_trends(DailyCounts) ->
    Sorted = lists:sort(fun({A, _}, {B, _}) -> A > B end, maps:to_list(DailyCounts)),
    [#{<<"date">> => Date, <<"count">> => Count} || {Date, Count} <- lists:sublist(Sorted, 30)].

format_hourly_trends(HourlyCounts) ->
    [#{<<"hour">> => H, <<"count">> => maps:get(integer_to_binary(H), HourlyCounts, 0)} || H <- lists:seq(0, 23)].

format_date({Y, M, D}) ->
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])).

format_datetime({{Y, M, D}, {H, Mi, S}}) ->
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y, M, D, H, Mi, S])).

bool_to_int(true) -> 1;
bool_to_int(false) -> 0.
