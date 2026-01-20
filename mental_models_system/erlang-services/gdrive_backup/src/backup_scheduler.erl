%%%-------------------------------------------------------------------
%%% @doc Backup Scheduler
%%% 
%%% Periodically creates backups and syncs with Google Drive.
%%% @end
%%%-------------------------------------------------------------------
-module(backup_scheduler).
-behaviour(gen_server).

-export([start_link/0, trigger_backup/0, set_interval/1, get_schedule/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_INTERVAL, 3600000). %% 1 hour in milliseconds

-record(state, {
    interval :: pos_integer(),
    timer_ref :: undefined | reference(),
    last_backup :: undefined | calendar:datetime(),
    next_backup :: undefined | calendar:datetime(),
    backup_count :: non_neg_integer()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

trigger_backup() ->
    gen_server:cast(?SERVER, trigger_backup).

set_interval(Seconds) when Seconds >= 60 ->
    gen_server:call(?SERVER, {set_interval, Seconds * 1000}).

get_schedule() ->
    gen_server:call(?SERVER, get_schedule).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Interval = application:get_env(gdrive_backup, backup_interval, 3600) * 1000,
    
    %% Schedule first backup check
    TimerRef = erlang:send_after(60000, self(), check_backup),
    
    {ok, #state{
        interval = Interval,
        timer_ref = TimerRef,
        last_backup = undefined,
        next_backup = add_milliseconds(calendar:local_time(), 60000),
        backup_count = 0
    }}.

handle_call({set_interval, NewInterval}, _From, State) ->
    %% Cancel existing timer
    case State#state.timer_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    
    %% Set new timer
    NewTimerRef = erlang:send_after(NewInterval, self(), check_backup),
    NewState = State#state{
        interval = NewInterval,
        timer_ref = NewTimerRef,
        next_backup = add_milliseconds(calendar:local_time(), NewInterval)
    },
    
    {reply, ok, NewState};

handle_call(get_schedule, _From, State) ->
    Schedule = #{
        <<"interval_seconds">> => State#state.interval div 1000,
        <<"last_backup">> => format_datetime(State#state.last_backup),
        <<"next_backup">> => format_datetime(State#state.next_backup),
        <<"backup_count">> => State#state.backup_count
    },
    {reply, {ok, Schedule}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(trigger_backup, State) ->
    do_backup(),
    NewState = State#state{
        last_backup = calendar:local_time(),
        backup_count = State#state.backup_count + 1
    },
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_backup, State) ->
    %% Perform backup
    do_backup(),
    
    %% Schedule next backup
    NewTimerRef = erlang:send_after(State#state.interval, self(), check_backup),
    
    NewState = State#state{
        timer_ref = NewTimerRef,
        last_backup = calendar:local_time(),
        next_backup = add_milliseconds(calendar:local_time(), State#state.interval),
        backup_count = State#state.backup_count + 1
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

do_backup() ->
    io:format("[~s] Starting scheduled backup...~n", [format_datetime(calendar:local_time())]),
    
    %% Create backup of important data
    BackupDir = "/data/backups",
    filelib:ensure_dir(BackupDir ++ "/"),
    
    Timestamp = integer_to_list(erlang:system_time(second)),
    BackupName = "mental_models_backup_" ++ Timestamp ++ ".tar.gz",
    BackupPath = BackupDir ++ "/" ++ BackupName,
    
    %% Create tarball of data directory (excluding backups themselves)
    Cmd = io_lib:format("cd /data && tar -czf ~s --exclude='backups' . 2>/dev/null || true", [BackupPath]),
    os:cmd(Cmd),
    
    case filelib:is_regular(BackupPath) of
        true ->
            {ok, Info} = file:read_file_info(BackupPath),
            Size = element(2, Info),
            io:format("[~s] Backup created: ~s (~p bytes)~n", 
                [format_datetime(calendar:local_time()), BackupName, Size]),
            
            %% Try to upload to Google Drive if configured
            case gdrive_client:get_status() of
                {ok, #{<<"gdrive_configured">> := true}} ->
                    gdrive_client:upload(BackupPath, list_to_binary(BackupName));
                _ ->
                    io:format("[~s] Google Drive not configured, backup stored locally only~n",
                        [format_datetime(calendar:local_time())])
            end,
            
            %% Cleanup old backups (keep last 10)
            cleanup_old_backups(BackupDir, 10);
        false ->
            io:format("[~s] Backup creation failed~n", [format_datetime(calendar:local_time())])
    end.

cleanup_old_backups(Dir, KeepCount) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            BackupFiles = lists:filter(fun(F) -> 
                lists:prefix("mental_models_backup_", F) 
            end, Files),
            
            case length(BackupFiles) > KeepCount of
                true ->
                    Sorted = lists:sort(BackupFiles),
                    ToDelete = lists:sublist(Sorted, length(Sorted) - KeepCount),
                    lists:foreach(fun(F) ->
                        file:delete(Dir ++ "/" ++ F)
                    end, ToDelete);
                false ->
                    ok
            end;
        {error, _} ->
            ok
    end.

add_milliseconds(DateTime, Ms) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
    NewSeconds = Seconds + (Ms div 1000),
    calendar:gregorian_seconds_to_datetime(NewSeconds).

format_datetime(undefined) -> <<"never">>;
format_datetime({{Y, M, D}, {H, Mi, S}}) ->
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B", [Y, M, D, H, Mi, S])).
