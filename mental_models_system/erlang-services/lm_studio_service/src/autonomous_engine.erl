%%%-------------------------------------------------------------------
%%% @doc Autonomous Engine - Self-directed research and improvement
%%% 
%%% The brain of the autonomous development system. This module:
%%% - Continuously analyzes the codebase for improvement opportunities
%%% - Self-directs research to fill knowledge gaps
%%% - Proposes and implements new features autonomously
%%% - Learns from successes and failures
%%% - Maintains a knowledge base that grows over time
%%% - Can run overnight without human intervention
%%%
%%% Design Philosophy:
%%% - Never stop running (bulletproof like the auto-updater)
%%% - Always make progress, even if small
%%% - Learn from every action taken
%%% - Prioritize safety (validate before deploy)
%%% - Blue-green deploy all changes
%%% @end
%%%-------------------------------------------------------------------
-module(autonomous_engine).
%% Helper modules: autonomous_engine_part2, autonomous_engine_part3, autonomous_engine_part4, autonomous_engine_part5
-behaviour(gen_server).

-export([start_link/0, get_status/0, force_cycle/0, set_mode/1, get_knowledge/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    mode = active :: active | paused | research_only | improve_only,
    cycle_count = 0 :: integer(),
    last_cycle = undefined :: undefined | integer(),
    timer_ref = undefined :: undefined | reference(),
    heartbeat_ref = undefined :: undefined | reference(),
    
    %% Research state
    research_queue = [] :: list(),
    current_research = undefined :: undefined | map(),
    completed_research = [] :: list(),
    
    %% Improvement state  
    improvement_queue = [] :: list(),
    current_improvement = undefined :: undefined | map(),
    completed_improvements = [] :: list(),
    failed_improvements = [] :: list(),
    
    %% Knowledge base
    knowledge_base = #{} :: map(),
    learned_patterns = [] :: list(),
    
    %% Statistics
    total_improvements = 0 :: integer(),
    successful_deploys = 0 :: integer(),
    failed_deploys = 0 :: integer(),
    research_completed = 0 :: integer()
}).

-define(CYCLE_INTERVAL, 300000).  %% 5 minutes between cycles
-define(HEARTBEAT_INTERVAL, 60000).  %% 1 minute heartbeat
-define(MAX_IMPROVEMENTS_PER_CYCLE, 3).
-define(KNOWLEDGE_FILE, "/data/knowledge_base.json").

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_status() ->
    gen_server:call(?MODULE, get_status).

force_cycle() ->
    gen_server:call(?MODULE, force_cycle, 300000).

set_mode(Mode) when Mode =:= active; Mode =:= paused; 
                    Mode =:= research_only; Mode =:= improve_only ->
    gen_server:call(?MODULE, {set_mode, Mode}).

get_knowledge() ->
    gen_server:call(?MODULE, get_knowledge).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[AUTONOMOUS] ========================================~n"),
    io:format("[AUTONOMOUS] AUTONOMOUS ENGINE STARTING~n"),
    io:format("[AUTONOMOUS] Self-directed research enabled~n"),
    io:format("[AUTONOMOUS] Continuous improvement active~n"),
    io:format("[AUTONOMOUS] ========================================~n"),
    
    %% Load existing knowledge base
    KnowledgeBase = load_knowledge_base(),
    
    %% Schedule first cycle and heartbeat
    TimerRef = erlang:send_after(?CYCLE_INTERVAL, self(), run_cycle),
    HeartbeatRef = erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat),
    
    %% Initialize research queue with default topics
    InitialResearch = [
        #{topic => <<"erlang_otp_best_practices">>, priority => high},
        #{topic => <<"blue_green_deployment_patterns">>, priority => high},
        #{topic => <<"self_healing_systems">>, priority => medium},
        #{topic => <<"mental_model_analysis">>, priority => medium},
        #{topic => <<"cowboy_optimization">>, priority => low}
    ],
    
    {ok, #state{
        timer_ref = TimerRef,
        heartbeat_ref = HeartbeatRef,
        research_queue = InitialResearch,
        knowledge_base = KnowledgeBase
    }}.

handle_call(get_status, _From, State) ->
    Status = #{
        <<"mode">> => State#state.mode,
        <<"cycle_count">> => State#state.cycle_count,
        <<"last_cycle">> => State#state.last_cycle,
        <<"research_queue_size">> => length(State#state.research_queue),
        <<"improvement_queue_size">> => length(State#state.improvement_queue),
        <<"current_research">> => State#state.current_research,
        <<"current_improvement">> => State#state.current_improvement,
        <<"total_improvements">> => State#state.total_improvements,
        <<"successful_deploys">> => State#state.successful_deploys,
        <<"failed_deploys">> => State#state.failed_deploys,
        <<"research_completed">> => State#state.research_completed,
        <<"learned_patterns">> => length(State#state.learned_patterns),
        <<"knowledge_topics">> => maps:keys(State#state.knowledge_base)
    },
    {reply, {ok, Status}, State};

handle_call(force_cycle, _From, State) ->
    io:format("[AUTONOMOUS] Forcing cycle~n"),
    NewState = run_autonomous_cycle(State),
    {reply, {ok, #{cycle => NewState#state.cycle_count}}, NewState};

handle_call({set_mode, Mode}, _From, State) ->
    io:format("[AUTONOMOUS] Mode changed to: ~p~n", [Mode]),
    {reply, ok, State#state{mode = Mode}};

handle_call(get_knowledge, _From, State) ->
    {reply, {ok, State#state.knowledge_base}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
