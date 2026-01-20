-module(improver_worker).
%% Helper modules: improver_worker_part2
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([get_status/0, trigger_improvement_cycle/0, get_improvements/0, get_improvement/1]).

-record(state, {
    status = idle,
    lm_studio_url,
    check_interval,
    last_check,
    improvements = [],
    pending_improvements = [],
    deployed_improvements = [],
    failed_improvements = [],
    auto_deploy,
    require_tests_pass,
    require_lint_pass,
    improvement_count = 0
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("[IMPROVER] Starting autonomous code improvement worker~n"),
    
    LmStudioUrl = application:get_env(code_improver_service, lm_studio_url, "http://host.docker.internal:1234"),
    CheckInterval = application:get_env(code_improver_service, check_interval, 300000),
    AutoDeploy = application:get_env(code_improver_service, auto_deploy, false),
    RequireTests = application:get_env(code_improver_service, require_tests_pass, true),
    RequireLint = application:get_env(code_improver_service, require_lint_pass, true),
    
    erlang:send_after(10000, self(), check_lm_studio),
    erlang:send_after(CheckInterval, self(), improvement_cycle),
    
    {ok, #state{
        lm_studio_url = LmStudioUrl,
        check_interval = CheckInterval,
        auto_deploy = AutoDeploy,
        require_tests_pass = RequireTests,
        require_lint_pass = RequireLint
    }}.

handle_call(get_status, _From, State) ->
    Status = #{
        status => State#state.status,
        lm_studio_url => list_to_binary(State#state.lm_studio_url),
        last_check => State#state.last_check,
        improvement_count => State#state.improvement_count,
        pending_count => length(State#state.pending_improvements),
        deployed_count => length(State#state.deployed_improvements),
        failed_count => length(State#state.failed_improvements),
        auto_deploy => State#state.auto_deploy
    },
    {reply, Status, State};

handle_call(get_improvements, _From, State) ->
    All = State#state.improvements ++ 
          State#state.pending_improvements ++ 
          State#state.deployed_improvements ++
          State#state.failed_improvements,
    {reply, All, State};

handle_call({get_improvement, Id}, _From, State) ->
    All = State#state.improvements ++ 
          State#state.pending_improvements ++ 
          State#state.deployed_improvements ++
          State#state.failed_improvements,
    Result = lists:filter(fun(I) -> maps:get(id, I, undefined) == Id end, All),
    {reply, Result, State};

handle_call(trigger_cycle, _From, State) ->
    self() ! improvement_cycle,
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_lm_studio, State) ->
    io:format("[IMPROVER] Checking LM Studio connection...~n"),
    case lm_studio_client:health_check(State#state.lm_studio_url) of
        {ok, healthy} ->
            io:format("[IMPROVER] LM Studio is healthy and ready~n");
        {error, Reason} ->
            io:format("[IMPROVER] LM Studio not available: ~p~n", [Reason]),
            io:format("[IMPROVER] Will retry on next cycle~n")
    end,
    {noreply, State};

handle_info(improvement_cycle, State) ->
    io:format("[IMPROVER] ========================================~n"),
    io:format("[IMPROVER] Starting improvement cycle~n"),
    io:format("[IMPROVER] ========================================~n"),
    
    NewState = run_improvement_cycle(State),
    
    erlang:send_after(State#state.check_interval, self(), improvement_cycle),
    {noreply, NewState#state{last_check = erlang:system_time(second)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

get_status() ->
    gen_server:call(?MODULE, get_status).

get_improvements() ->
    gen_server:call(?MODULE, get_improvements).

get_improvement(Id) ->
    gen_server:call(?MODULE, {get_improvement, Id}).

trigger_improvement_cycle() ->
    gen_server:call(?MODULE, trigger_cycle).

run_improvement_cycle(State) ->
    case lm_studio_client:health_check(State#state.lm_studio_url) of
        {ok, healthy} ->
            io:format("[IMPROVER] LM Studio connected, scanning for improvements...~n"),
            run_improvement_scan(State);
        {error, Reason} ->
            io:format("[IMPROVER] LM Studio not available: ~p, skipping cycle~n", [Reason]),
            State
    end.
