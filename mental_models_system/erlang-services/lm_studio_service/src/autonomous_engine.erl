%%%-------------------------------------------------------------------
%%% @doc Autonomous Engine - Self-directed research and improvement
%%% 
%%% The brain of the autonomous development system. Orchestrates:
%%% - autonomous_analyzer: Codebase analysis
%%% - autonomous_research: Self-directed research
%%% - autonomous_improver: Code improvement generation
%%% - autonomous_deployer: Validation and deployment
%%% - autonomous_learner: Learning and knowledge persistence
%%% @end
%%%-------------------------------------------------------------------
-module(autonomous_engine).
-behaviour(gen_server).

-export([start_link/0, get_status/0, force_cycle/0, set_mode/1, get_knowledge/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    mode = active :: active | paused | research_only | improve_only,
    cycle_count = 0 :: integer(),
    last_cycle = undefined :: undefined | integer(),
    timer_ref = undefined :: undefined | reference(),
    heartbeat_ref = undefined :: undefined | reference(),
    research_queue = [] :: list(),
    completed_research = [] :: list(),
    improvement_queue = [] :: list(),
    completed_improvements = [] :: list(),
    failed_improvements = [] :: list(),
    knowledge_base = #{} :: map(),
    learned_patterns = [] :: list(),
    total_improvements = 0 :: integer(),
    successful_deploys = 0 :: integer(),
    failed_deploys = 0 :: integer(),
    research_completed = 0 :: integer()
}).

-define(CYCLE_INTERVAL, 300000).
-define(HEARTBEAT_INTERVAL, 60000).

%%====================================================================
%% API
%%====================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
get_status() -> gen_server:call(?MODULE, get_status).
force_cycle() -> gen_server:call(?MODULE, force_cycle, 300000).
set_mode(Mode) when Mode =:= active; Mode =:= paused; 
                    Mode =:= research_only; Mode =:= improve_only ->
    gen_server:call(?MODULE, {set_mode, Mode}).
get_knowledge() -> gen_server:call(?MODULE, get_knowledge).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[AUTONOMOUS] Engine starting~n"),
    KnowledgeBase = autonomous_learner:load_knowledge_base(),
    TimerRef = erlang:send_after(?CYCLE_INTERVAL, self(), run_cycle),
    HeartbeatRef = erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat),
    InitialResearch = [
        #{topic => <<"erlang_otp_best_practices">>, priority => high},
        #{topic => <<"blue_green_deployment_patterns">>, priority => high}
    ],
    {ok, #state{timer_ref = TimerRef, heartbeat_ref = HeartbeatRef,
                research_queue = InitialResearch, knowledge_base = KnowledgeBase}}.

handle_call(get_status, _From, State) ->
    Status = #{
        <<"mode">> => State#state.mode,
        <<"cycle_count">> => State#state.cycle_count,
        <<"last_cycle">> => State#state.last_cycle,
        <<"research_queue_size">> => length(State#state.research_queue),
        <<"improvement_queue_size">> => length(State#state.improvement_queue),
        <<"total_improvements">> => State#state.total_improvements,
        <<"successful_deploys">> => State#state.successful_deploys,
        <<"failed_deploys">> => State#state.failed_deploys,
        <<"research_completed">> => State#state.research_completed
    },
    {reply, {ok, Status}, State};

handle_call(force_cycle, _From, State) ->
    NewState = run_autonomous_cycle(State),
    {reply, {ok, #{cycle => NewState#state.cycle_count}}, NewState};

handle_call({set_mode, Mode}, _From, State) ->
    io:format("[AUTONOMOUS] Mode: ~p~n", [Mode]),
    {reply, ok, State#state{mode = Mode}};

handle_call(get_knowledge, _From, State) ->
    {reply, {ok, State#state.knowledge_base}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(heartbeat, State) ->
    HeartbeatRef = erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat),
    io:format("[AUTONOMOUS] HEARTBEAT - Cycles: ~p~n", [State#state.cycle_count]),
    NewTimerRef = ensure_timer_running(State#state.timer_ref),
    {noreply, State#state{heartbeat_ref = HeartbeatRef, timer_ref = NewTimerRef}};

handle_info(run_cycle, State) ->
    TimerRef = erlang:send_after(?CYCLE_INTERVAL, self(), run_cycle),
    NewState = case State#state.mode of
        paused -> State;
        _ -> run_autonomous_cycle(State)
    end,
    {noreply, NewState#state{timer_ref = TimerRef}};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, State) ->
    autonomous_learner:save_knowledge_base(State#state.knowledge_base),
    ok.

%%====================================================================
%% Main Cycle
%%====================================================================

run_autonomous_cycle(State) ->
    io:format("[AUTONOMOUS] CYCLE #~p~n", [State#state.cycle_count + 1]),
    try
        %% Phase 1: Analyze
        Analysis = autonomous_analyzer:analyze_codebase(),
        
        %% Phase 2: Research
        State2 = case State#state.mode of
            improve_only -> State;
            _ -> do_research_phase(State, Analysis)
        end,
        
        %% Phase 3: Generate improvements
        State3 = case State#state.mode of
            research_only -> State2;
            _ -> do_improvement_phase(State2, Analysis)
        end,
        
        %% Phase 4: Deploy
        State4 = case State#state.mode of
            research_only -> State3;
            _ -> do_deploy_phase(State3)
        end,
        
        %% Phase 5: Learn
        State5 = do_learn_phase(State4),
        
        autonomous_learner:save_knowledge_base(State5#state.knowledge_base),
        State5#state{cycle_count = State#state.cycle_count + 1,
                     last_cycle = erlang:system_time(second)}
    catch
        Class:Reason:Stack ->
            io:format("[AUTONOMOUS] ERROR: ~p:~p~n~p~n", [Class, Reason, Stack]),
            State#state{cycle_count = State#state.cycle_count + 1}
    end.

do_research_phase(State, Analysis) ->
    {NewQueue, NewKB, NewCount, _Completed} = 
        autonomous_research:conduct_research(
            State#state.research_queue, 
            State#state.knowledge_base,
            State#state.research_completed),
    NewQueue2 = case NewQueue of
        [] -> autonomous_research:generate_research_topics(Analysis, NewKB);
        _ -> NewQueue
    end,
    State#state{research_queue = NewQueue2, knowledge_base = NewKB, 
                research_completed = NewCount}.

do_improvement_phase(State, Analysis) ->
    Improvements = autonomous_improver:generate_improvements(
        Analysis, State#state.knowledge_base),
    State#state{improvement_queue = Improvements}.

do_deploy_phase(State) ->
    {Deployed, Failed} = autonomous_deployer:validate_and_deploy(
        State#state.improvement_queue),
    State#state{
        improvement_queue = [],
        completed_improvements = Deployed ++ State#state.completed_improvements,
        failed_improvements = Failed ++ State#state.failed_improvements,
        total_improvements = State#state.total_improvements + length(Deployed),
        successful_deploys = State#state.successful_deploys + length(Deployed),
        failed_deploys = State#state.failed_deploys + length(Failed)
    }.

do_learn_phase(State) ->
    NewPatterns = autonomous_learner:learn_from_cycle(
        State#state.completed_improvements, State#state.failed_improvements),
    State#state{learned_patterns = NewPatterns ++ State#state.learned_patterns}.

ensure_timer_running(undefined) ->
    erlang:send_after(?CYCLE_INTERVAL, self(), run_cycle);
ensure_timer_running(Ref) ->
    case erlang:read_timer(Ref) of
        false -> erlang:send_after(?CYCLE_INTERVAL, self(), run_cycle);
        _ -> Ref
    end.
