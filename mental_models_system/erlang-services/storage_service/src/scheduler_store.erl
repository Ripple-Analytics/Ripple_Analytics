%%%-------------------------------------------------------------------
%%% @doc Scheduler Store - Persistent storage for scheduled analysis tasks
%%% Uses DETS for persistence across restarts.
%%% @end
%%%-------------------------------------------------------------------
-module(scheduler_store).
-behaviour(gen_server).

-export([start_link/0, add_task/1, get_task/1, list_tasks/0, 
         update_task/2, delete_task/1, get_due_tasks/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(TABLE, scheduled_tasks).
-define(SERVER, ?MODULE).

-record(scheduled_task, {
    id,
    name,
    type,           %% url | text
    target,         %% URL or text to analyze
    interval,       %% seconds between runs
    last_run,       %% timestamp of last run
    next_run,       %% timestamp of next run
    enabled,        %% true | false
    created_at,
    results_count   %% number of times this task has run
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_task(TaskData) ->
    gen_server:call(?SERVER, {add_task, TaskData}).

get_task(Id) ->
    gen_server:call(?SERVER, {get_task, Id}).

list_tasks() ->
    gen_server:call(?SERVER, list_tasks).

update_task(Id, Updates) ->
    gen_server:call(?SERVER, {update_task, Id, Updates}).

delete_task(Id) ->
    gen_server:call(?SERVER, {delete_task, Id}).

get_due_tasks() ->
    gen_server:call(?SERVER, get_due_tasks).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    DataDir = application:get_env(storage_service, data_dir, "/data"),
    filelib:ensure_dir(DataDir ++ "/"),
    TableFile = filename:join(DataDir, "scheduled_tasks.dets"),
    
    case dets:open_file(?TABLE, [{file, TableFile}, {type, set}]) of
        {ok, ?TABLE} ->
            io:format("[SCHEDULER_STORE] Opened scheduled tasks table: ~s~n", [TableFile]),
            {ok, #{}};
        {error, Reason} ->
            io:format("[SCHEDULER_STORE] Failed to open table: ~p~n", [Reason]),
            {stop, Reason}
    end.

handle_call({add_task, TaskData}, _From, State) ->
    Id = generate_id(),
    Now = erlang:system_time(second),
    Interval = maps:get(interval, TaskData, 86400),
    
    Task = #scheduled_task{
        id = Id,
        name = maps:get(name, TaskData, <<"Unnamed Task">>),
        type = maps:get(type, TaskData, url),
        target = maps:get(target, TaskData, <<>>),
        interval = Interval,
        last_run = 0,
        next_run = Now,  %% Run immediately on first schedule
        enabled = true,
        created_at = Now,
        results_count = 0
    },
    
    ok = dets:insert(?TABLE, {Id, Task}),
    {reply, {ok, Id}, State};

handle_call({get_task, Id}, _From, State) ->
    case dets:lookup(?TABLE, Id) of
        [{Id, Task}] -> {reply, {ok, task_to_map(Task)}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call(list_tasks, _From, State) ->
    Tasks = dets:foldl(
        fun({_Id, Task}, Acc) -> [task_to_map(Task) | Acc] end,
        [],
        ?TABLE
    ),
    SortedTasks = lists:sort(
        fun(A, B) -> maps:get(created_at, A) > maps:get(created_at, B) end,
        Tasks
    ),
    {reply, {ok, SortedTasks}, State};

handle_call({update_task, Id, Updates}, _From, State) ->
    case dets:lookup(?TABLE, Id) of
        [{Id, Task}] ->
            UpdatedTask = apply_updates(Task, Updates),
            ok = dets:insert(?TABLE, {Id, UpdatedTask}),
            {reply, ok, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_task, Id}, _From, State) ->
    ok = dets:delete(?TABLE, Id),
    {reply, ok, State};

handle_call(get_due_tasks, _From, State) ->
    Now = erlang:system_time(second),
    DueTasks = dets:foldl(
        fun({_Id, Task}, Acc) ->
            case Task#scheduled_task.enabled andalso Task#scheduled_task.next_run =< Now of
                true -> [task_to_map(Task) | Acc];
                false -> Acc
            end
        end,
        [],
        ?TABLE
    ),
    {reply, {ok, DueTasks}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    dets:close(?TABLE),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

generate_id() ->
    list_to_binary(integer_to_list(erlang:system_time(microsecond))).

task_to_map(#scheduled_task{} = T) ->
    #{
        id => T#scheduled_task.id,
        name => T#scheduled_task.name,
        type => T#scheduled_task.type,
        target => T#scheduled_task.target,
        interval => T#scheduled_task.interval,
        last_run => T#scheduled_task.last_run,
        next_run => T#scheduled_task.next_run,
        enabled => T#scheduled_task.enabled,
        created_at => T#scheduled_task.created_at,
        results_count => T#scheduled_task.results_count
    }.

apply_updates(Task, Updates) ->
    Task#scheduled_task{
        name = maps:get(name, Updates, Task#scheduled_task.name),
        type = maps:get(type, Updates, Task#scheduled_task.type),
        target = maps:get(target, Updates, Task#scheduled_task.target),
        interval = maps:get(interval, Updates, Task#scheduled_task.interval),
        last_run = maps:get(last_run, Updates, Task#scheduled_task.last_run),
        next_run = maps:get(next_run, Updates, Task#scheduled_task.next_run),
        enabled = maps:get(enabled, Updates, Task#scheduled_task.enabled),
        results_count = maps:get(results_count, Updates, Task#scheduled_task.results_count)
    }.
