%%%-------------------------------------------------------------------
%%% @doc Updater Worker - Proper Blue-Green Deployment
%%% 
%%% CORE PRINCIPLE: Active version NEVER touched during update.
%%% Only the standby is rebuilt. If standby fails, active keeps serving.
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(updater_worker).
%% Helper modules: updater_worker_part2, updater_worker_part3, updater_worker_part4, updater_worker_part5, updater_worker_part6
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([get_status/0, check_updates/0, perform_update/0, get_config/0]).

-record(state, {
    status = idle :: idle | checking | updating | error,
    last_check = undefined,
    last_update = undefined,
    current_commit = undefined,
    remote_commit = undefined,
    branch = <<"release2">>,
    update_available = false,
    active_env = "blue",
    standby_env = "green",
    error_message = undefined,
    check_interval = 300000,
    timer_ref = undefined,
    heartbeat_ref = undefined,
    check_count = 0,
    consecutive_failures = 0,
    last_failed_commit = undefined
}).

-define(MAX_CONSECUTIVE_FAILURES, 3).
-define(DATA_DIR, "/data").

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_status() -> gen_server:call(?MODULE, get_status).
check_updates() -> gen_server:call(?MODULE, check_updates, 60000).
perform_update() -> gen_server:call(?MODULE, perform_update, 600000).
get_config() -> gen_server:call(?MODULE, get_config).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[UPDATER] ========================================~n"),
    io:format("[UPDATER] BLUE-GREEN AUTO-UPDATER v2.0~n"),
    io:format("[UPDATER] Active stays running, only standby updated~n"),
    io:format("[UPDATER] ========================================~n"),
    
    %% Ensure data directory exists
    filelib:ensure_dir(?DATA_DIR ++ "/"),
    
    Interval = get_check_interval(),
    %% HARDCODED - don't rely on env vars that might be wrong
    Branch = <<"release2">>,
    ActiveEnv = read_active_env(),
    StandbyEnv = case ActiveEnv of "blue" -> "green"; _ -> "blue" end,
    
    io:format("[UPDATER] Branch: ~s~n", [Branch]),
    io:format("[UPDATER] Active: ~s, Standby: ~s~n", [ActiveEnv, StandbyEnv]),
    
    self() ! init_repo,
    %% Check immediately on startup, then every Interval after
    TimerRef = erlang:send_after(5000, self(), check_timer),  %% First check in 5 seconds
    HeartbeatRef = erlang:send_after(60000, self(), heartbeat),
    
    {ok, #state{
        check_interval = Interval, 
        timer_ref = TimerRef, 
        heartbeat_ref = HeartbeatRef,
        branch = Branch,
        active_env = ActiveEnv,
        standby_env = StandbyEnv
    }}.

handle_call(get_status, _From, State) ->
    Status = #{
        status => State#state.status,
        last_check => format_time(State#state.last_check),
        last_update => format_time(State#state.last_update),
        current_commit => State#state.current_commit,
        remote_commit => State#state.remote_commit,
        branch => State#state.branch,
        update_available => State#state.update_available,
        active_env => list_to_binary(State#state.active_env),
        standby_env => list_to_binary(State#state.standby_env),
        consecutive_failures => State#state.consecutive_failures,
        error_message => State#state.error_message
    },
    {reply, {ok, Status}, State};

handle_call(check_updates, _From, State) ->
    NewState = do_check_updates(State),
    {reply, {ok, #{update_available => NewState#state.update_available}}, NewState};

handle_call(perform_update, _From, State) ->
    NewState = do_perform_update(State),
    Result = case NewState#state.status of
        idle -> {ok, #{message => <<"Update successful">>}};
        error -> {error, #{message => NewState#state.error_message}};
        _ -> {ok, #{message => <<"Update in progress">>}}
    end,
    {reply, Result, NewState};

handle_call(get_config, _From, State) ->
    Config = #{
        branch => State#state.branch,
        active_env => list_to_binary(State#state.active_env),
        standby_env => list_to_binary(State#state.standby_env),
        check_interval_seconds => State#state.check_interval div 1000
    },
    {reply, {ok, Config}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init_repo, State) ->
    NewState = try
        do_init_repo(State)
    catch
        _:_ -> State#state{status = error, error_message = <<"Init failed">>}
    end,
    {noreply, NewState};

handle_info(heartbeat, State) ->
    HeartbeatRef = erlang:send_after(60000, self(), heartbeat),
    io:format("[UPDATER] HEARTBEAT | Active: ~s | Checks: ~p | Failures: ~p~n", 
              [State#state.active_env, State#state.check_count, State#state.consecutive_failures]),
    %% Phone home - report status to external webhook
    phone_home(State),
    {noreply, State#state{heartbeat_ref = HeartbeatRef}};
