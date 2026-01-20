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

handle_info(heartbeat, State) ->
    %% CRITICAL: Always reschedule heartbeat FIRST
    HeartbeatRef = erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat),
    
    io:format("[AUTONOMOUS] HEARTBEAT - Mode: ~p | Cycles: ~p | Improvements: ~p | Research: ~p~n",
              [State#state.mode, State#state.cycle_count, 
               State#state.total_improvements, State#state.research_completed]),
    
    %% Watchdog: ensure timer is running
    NewTimerRef = ensure_timer_running(State#state.timer_ref),
    
    {noreply, State#state{heartbeat_ref = HeartbeatRef, timer_ref = NewTimerRef}};

handle_info(run_cycle, State) ->
    %% CRITICAL: Always reschedule timer FIRST
    TimerRef = erlang:send_after(?CYCLE_INTERVAL, self(), run_cycle),
    
    NewState = case State#state.mode of
        paused ->
            io:format("[AUTONOMOUS] Cycle skipped (paused mode)~n"),
            State;
        _ ->
            run_autonomous_cycle(State)
    end,
    
    {noreply, NewState#state{timer_ref = TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Save knowledge base on shutdown
    save_knowledge_base(State#state.knowledge_base),
    ok.

%%====================================================================
%% Autonomous Cycle - The main brain loop
%%====================================================================

run_autonomous_cycle(State) ->
    io:format("[AUTONOMOUS] ========================================~n"),
    io:format("[AUTONOMOUS] CYCLE #~p STARTING~n", [State#state.cycle_count + 1]),
    io:format("[AUTONOMOUS] ========================================~n"),
    
    try
        %% Phase 1: Analyze current state
        io:format("[AUTONOMOUS] Phase 1: Analyzing codebase...~n"),
        AnalysisResult = analyze_codebase(),
        
        %% Phase 2: Self-directed research
        State2 = case State#state.mode of
            improve_only -> State;
            _ ->
                io:format("[AUTONOMOUS] Phase 2: Conducting research...~n"),
                conduct_research(State, AnalysisResult)
        end,
        
        %% Phase 3: Generate improvements
        State3 = case State#state.mode of
            research_only -> State2;
            _ ->
                io:format("[AUTONOMOUS] Phase 3: Generating improvements...~n"),
                generate_improvements(State2, AnalysisResult)
        end,
        
        %% Phase 4: Validate and deploy
        State4 = case State#state.mode of
            research_only -> State3;
            _ ->
                io:format("[AUTONOMOUS] Phase 4: Validating and deploying...~n"),
                validate_and_deploy_improvements(State3)
        end,
        
        %% Phase 5: Learn from results
        io:format("[AUTONOMOUS] Phase 5: Learning from results...~n"),
        State5 = learn_from_cycle(State4),
        
        %% Save knowledge periodically
        save_knowledge_base(State5#state.knowledge_base),
        
        io:format("[AUTONOMOUS] CYCLE #~p COMPLETE~n", [State5#state.cycle_count]),
        io:format("[AUTONOMOUS] ========================================~n"),
        
        State5#state{
            cycle_count = State#state.cycle_count + 1,
            last_cycle = erlang:system_time(second)
        }
    catch
        Class:Reason:Stacktrace ->
            io:format("[AUTONOMOUS] ERROR in cycle: ~p:~p~n~p~n", [Class, Reason, Stacktrace]),
            io:format("[AUTONOMOUS] Continuing to next cycle...~n"),
            State#state{
                cycle_count = State#state.cycle_count + 1,
                last_cycle = erlang:system_time(second)
            }
    end.

%%====================================================================
%% Phase 1: Codebase Analysis
%%====================================================================

analyze_codebase() ->
    BasePath = "/repo/mental_models_system/erlang-services",
    
    %% Find all Erlang files
    ErlFiles = filelib:wildcard(BasePath ++ "/**/src/*.erl"),
    
    %% Analyze each file for issues and opportunities
    Analysis = lists:map(fun(File) ->
        analyze_file(File)
    end, ErlFiles),
    
    %% Aggregate results
    #{
        <<"total_files">> => length(ErlFiles),
        <<"files_with_issues">> => length([A || A <- Analysis, has_issues(A)]),
        <<"improvement_opportunities">> => extract_opportunities(Analysis),
        <<"file_analysis">> => Analysis
    }.

analyze_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            Lines = binary:split(Content, <<"\n">>, [global]),
            #{
                <<"file">> => list_to_binary(FilePath),
                <<"lines">> => length(Lines),
                <<"has_todos">> => has_pattern(Content, <<"TODO">>),
                <<"has_fixmes">> => has_pattern(Content, <<"FIXME">>),
                <<"missing_docs">> => not has_pattern(Content, <<"@doc">>),
                <<"long_functions">> => count_long_functions(Content),
                <<"complexity_score">> => estimate_complexity(Content)
            };
        {error, _} ->
            #{<<"file">> => list_to_binary(FilePath), <<"error">> => <<"read_failed">>}
    end.

has_issues(#{<<"has_todos">> := true}) -> true;
has_issues(#{<<"has_fixmes">> := true}) -> true;
has_issues(#{<<"missing_docs">> := true}) -> true;
has_issues(#{<<"long_functions">> := N}) when N > 0 -> true;
has_issues(_) -> false.

has_pattern(Content, Pattern) ->
    binary:match(Content, Pattern) =/= nomatch.

count_long_functions(Content) ->
    %% Simple heuristic: count functions > 50 lines
    %% This is approximate but useful for prioritization
    Functions = binary:split(Content, <<".\n">>, [global]),
    length([F || F <- Functions, byte_size(F) > 2000]).

estimate_complexity(Content) ->
    %% Simple complexity score based on control structures
    CaseCount = length(binary:matches(Content, <<"case">>)),
    IfCount = length(binary:matches(Content, <<"if">>)),
    TryCount = length(binary:matches(Content, <<"try">>)),
    CaseCount + IfCount + TryCount.

extract_opportunities(Analysis) ->
    lists:filtermap(fun(A) ->
        case has_issues(A) of
            true -> {true, maps:get(<<"file">>, A, <<>>)};
            false -> false
        end
    end, Analysis).

%%====================================================================
%% Phase 2: Self-Directed Research
%%====================================================================

conduct_research(State, AnalysisResult) ->
    case State#state.research_queue of
        [] ->
            %% Generate new research topics based on analysis
            NewTopics = generate_research_topics(AnalysisResult, State#state.knowledge_base),
            State#state{research_queue = NewTopics};
        [Topic | Rest] ->
            %% Conduct research on the topic
            case do_research(Topic, State#state.knowledge_base) of
                {ok, Findings} ->
                    %% Add findings to knowledge base
                    TopicKey = maps:get(topic, Topic),
                    NewKB = maps:put(TopicKey, Findings, State#state.knowledge_base),
                    State#state{
                        research_queue = Rest,
                        completed_research = [#{topic => Topic, findings => Findings} | 
                                              State#state.completed_research],
                        knowledge_base = NewKB,
                        research_completed = State#state.research_completed + 1
                    };
                {error, _Reason} ->
                    %% Move to end of queue and try later
                    State#state{research_queue = Rest ++ [Topic]}
            end
    end.

generate_research_topics(AnalysisResult, KnowledgeBase) ->
    %% Identify gaps in knowledge based on codebase analysis
    Opportunities = maps:get(<<"improvement_opportunities">>, AnalysisResult, []),
    ExistingTopics = maps:keys(KnowledgeBase),
    
    %% Generate topics for areas we don't know about yet
    BaseTopics = [
        #{topic => <<"error_handling_patterns">>, priority => high},
        #{topic => <<"gen_server_optimization">>, priority => medium},
        #{topic => <<"cowboy_handler_patterns">>, priority => medium},
        #{topic => <<"ets_best_practices">>, priority => low},
        #{topic => <<"supervision_tree_design">>, priority => high}
    ],
    
    %% Filter out topics we already know
    [T || T <- BaseTopics, not lists:member(maps:get(topic, T), ExistingTopics)].

do_research(Topic, _ExistingKnowledge) ->
    TopicName = maps:get(topic, Topic),
    io:format("[AUTONOMOUS] Researching: ~s~n", [TopicName]),
    
    %% Build research prompt
    Prompt = build_research_prompt(TopicName),
    SystemPrompt = "You are an expert Erlang/OTP developer and system architect. "
                   "Provide detailed, actionable knowledge about the requested topic. "
                   "Focus on best practices, common patterns, and practical implementation advice. "
                   "Format your response as JSON with keys: summary, best_practices (list), "
                   "common_patterns (list), pitfalls_to_avoid (list), code_examples (list).",
    
    case lm_client:generate(Prompt, SystemPrompt) of
        {ok, Response} ->
            %% Parse the research findings
            parse_research_response(Response);
        {error, Reason} ->
            {error, Reason}
    end.

build_research_prompt(TopicName) ->
    "Please provide comprehensive knowledge about: " ++ binary_to_list(TopicName) ++ "\n\n"
    "Context: This is for an Erlang/OTP microservices system with:\n"
    "- Cowboy HTTP handlers\n"
    "- gen_server processes\n"
    "- Blue-green deployments\n"
    "- Docker containers\n"
    "- Self-healing capabilities\n\n"
    "Provide practical, implementable advice.".

parse_research_response(Response) ->
    try
        Decoded = jsx:decode(Response, [return_maps]),
        {ok, Decoded}
    catch
        _:_ ->
            %% If not valid JSON, wrap the response
            {ok, #{
                <<"summary">> => Response,
                <<"best_practices">> => [],
                <<"common_patterns">> => [],
                <<"pitfalls_to_avoid">> => [],
                <<"code_examples">> => []
            }}
    end.

%%====================================================================
%% Phase 3: Generate Improvements
%%====================================================================

generate_improvements(State, AnalysisResult) ->
    Opportunities = maps:get(<<"improvement_opportunities">>, AnalysisResult, []),
    
    %% Take top N opportunities
    TargetFiles = lists:sublist(Opportunities, ?MAX_IMPROVEMENTS_PER_CYCLE),
    
    %% Generate improvements for each
    Improvements = lists:filtermap(fun(FilePath) ->
        case generate_improvement_for_file(FilePath, State#state.knowledge_base) of
            {ok, Improvement} -> {true, Improvement};
            {error, _} -> false
        end
    end, TargetFiles),
    
    State#state{improvement_queue = Improvements}.

generate_improvement_for_file(FilePath, KnowledgeBase) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            %% Build improvement prompt with knowledge context
            Prompt = build_improvement_prompt(FilePath, Content, KnowledgeBase),
            SystemPrompt = get_improvement_system_prompt(),
            
            case lm_client:generate(Prompt, SystemPrompt) of
                {ok, Response} ->
                    parse_improvement_response(FilePath, Response);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

build_improvement_prompt(FilePath, Content, KnowledgeBase) ->
    %% Include relevant knowledge in the prompt
    RelevantKnowledge = extract_relevant_knowledge(FilePath, KnowledgeBase),
    
    "Analyze this Erlang file and suggest ONE specific improvement.\n\n"
    "File: " ++ binary_to_list(FilePath) ++ "\n\n"
    "Relevant best practices:\n" ++ format_knowledge(RelevantKnowledge) ++ "\n\n"
    "Current code:\n```erlang\n" ++ binary_to_list(Content) ++ "\n```\n\n"
    "Respond with JSON containing:\n"
    "- description: What the improvement does\n"
    "- old_code: The exact code to replace (must match exactly)\n"
    "- new_code: The improved code\n"
    "- benefit: Why this is better\n"
    "- risk_level: low/medium/high".

get_improvement_system_prompt() ->
    "You are an expert Erlang/OTP developer specializing in code improvement. "
    "Your improvements must be:\n"
    "1. Safe - no breaking changes\n"
    "2. Incremental - small, focused changes\n"
    "3. Testable - the code must compile\n"
    "4. Beneficial - clear improvement in quality, performance, or maintainability\n\n"
    "Always respond with valid JSON. The old_code must match EXACTLY what's in the file.".

extract_relevant_knowledge(FilePath, KnowledgeBase) ->
    %% Determine what knowledge is relevant based on file content
    FileType = determine_file_type(FilePath),
    RelevantKeys = case FileType of
        handler -> [<<"cowboy_handler_patterns">>, <<"error_handling_patterns">>];
        gen_server -> [<<"gen_server_optimization">>, <<"supervision_tree_design">>];
        _ -> [<<"erlang_otp_best_practices">>]
    end,
    
    maps:with(RelevantKeys, KnowledgeBase).

determine_file_type(FilePath) ->
    case binary:match(FilePath, <<"_handler.erl">>) of
        nomatch ->
            case binary:match(FilePath, <<"_sup.erl">>) of
                nomatch -> other;
                _ -> supervisor
            end;
        _ -> handler
    end.

format_knowledge(KnowledgeMap) when map_size(KnowledgeMap) =:= 0 ->
    "No specific knowledge available yet.";
format_knowledge(KnowledgeMap) ->
    lists:flatten([
        io_lib:format("~s: ~p~n", [K, V]) 
        || {K, V} <- maps:to_list(KnowledgeMap)
    ]).

parse_improvement_response(FilePath, Response) ->
    try
        Decoded = jsx:decode(Response, [return_maps]),
        {ok, #{
            <<"file">> => FilePath,
            <<"description">> => maps:get(<<"description">>, Decoded, <<>>),
            <<"old_code">> => maps:get(<<"old_code">>, Decoded, <<>>),
            <<"new_code">> => maps:get(<<"new_code">>, Decoded, <<>>),
            <<"benefit">> => maps:get(<<"benefit">>, Decoded, <<>>),
            <<"risk_level">> => maps:get(<<"risk_level">>, Decoded, <<"medium">>),
            <<"timestamp">> => erlang:system_time(second)
        }}
    catch
        _:_ ->
            {error, parse_failed}
    end.

%%====================================================================
%% Phase 4: Validate and Deploy
%%====================================================================

validate_and_deploy_improvements(State) ->
    %% Process each improvement in the queue
    {Deployed, Failed} = lists:foldl(fun(Improvement, {DepAcc, FailAcc}) ->
        case validate_improvement(Improvement) of
            {ok, validated} ->
                case deploy_improvement(Improvement) of
                    {ok, _} -> 
                        {[Improvement | DepAcc], FailAcc};
                    {error, Reason} -> 
                        {DepAcc, [{Improvement, Reason} | FailAcc]}
                end;
            {error, Reason} ->
                {DepAcc, [{Improvement, Reason} | FailAcc]}
        end
    end, {[], []}, State#state.improvement_queue),
    
    State#state{
        improvement_queue = [],
        completed_improvements = Deployed ++ State#state.completed_improvements,
        failed_improvements = Failed ++ State#state.failed_improvements,
        total_improvements = State#state.total_improvements + length(Deployed),
        successful_deploys = State#state.successful_deploys + length(Deployed),
        failed_deploys = State#state.failed_deploys + length(Failed)
    }.

validate_improvement(Improvement) ->
    %% Check risk level
    RiskLevel = maps:get(<<"risk_level">>, Improvement, <<"medium">>),
    case RiskLevel of
        <<"high">> ->
            io:format("[AUTONOMOUS] Skipping high-risk improvement~n"),
            {error, high_risk};
        _ ->
            %% Validate the code compiles
            validate_code_compiles(Improvement)
    end.

validate_code_compiles(Improvement) ->
    NewCode = maps:get(<<"new_code">>, Improvement, <<>>),
    
    %% Write to temp file and try to compile
    TempFile = "/tmp/validate_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".erl",
    
    %% We need a complete module to compile, so wrap the code
    TestModule = <<"-module(validate_temp).\n", NewCode/binary>>,
    
    case file:write_file(TempFile, TestModule) of
        ok ->
            Result = os:cmd("erlc -W " ++ TempFile ++ " 2>&1"),
            file:delete(TempFile),
            file:delete("/tmp/validate_temp.beam"),
            
            case string:find(Result, "error") of
                nomatch -> {ok, validated};
                _ -> {error, {compile_error, Result}}
            end;
        {error, Reason} ->
            {error, {write_failed, Reason}}
    end.

deploy_improvement(Improvement) ->
    FilePath = binary_to_list(maps:get(<<"file">>, Improvement)),
    OldCode = maps:get(<<"old_code">>, Improvement),
    NewCode = maps:get(<<"new_code">>, Improvement),
    
    case file:read_file(FilePath) of
        {ok, Content} ->
            case binary:match(Content, OldCode) of
                nomatch ->
                    {error, old_code_not_found};
                _ ->
                    UpdatedContent = binary:replace(Content, OldCode, NewCode),
                    case file:write_file(FilePath, UpdatedContent) of
                        ok ->
                            io:format("[AUTONOMOUS] Deployed improvement to ~s~n", [FilePath]),
                            {ok, deployed};
                        {error, Reason} ->
                            {error, {write_failed, Reason}}
                    end
            end;
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.

%%====================================================================
%% Phase 5: Learning
%%====================================================================

learn_from_cycle(State) ->
    %% Analyze what worked and what didn't
    SuccessPatterns = extract_success_patterns(State#state.completed_improvements),
    FailurePatterns = extract_failure_patterns(State#state.failed_improvements),
    
    %% Update learned patterns
    NewPatterns = SuccessPatterns ++ FailurePatterns,
    
    State#state{
        learned_patterns = NewPatterns ++ State#state.learned_patterns
    }.

extract_success_patterns(Improvements) ->
    [#{
        type => success,
        file_type => determine_file_type(maps:get(<<"file">>, I, <<>>)),
        description => maps:get(<<"description">>, I, <<>>)
    } || I <- Improvements].

extract_failure_patterns(Failures) ->
    [#{
        type => failure,
        reason => Reason
    } || {_Improvement, Reason} <- Failures].

%%====================================================================
%% Knowledge Base Persistence
%%====================================================================

load_knowledge_base() ->
    case file:read_file(?KNOWLEDGE_FILE) of
        {ok, Content} ->
            try jsx:decode(Content, [return_maps])
            catch _:_ -> #{}
            end;
        {error, _} ->
            #{}
    end.

save_knowledge_base(KnowledgeBase) ->
    %% Ensure directory exists
    filelib:ensure_dir(?KNOWLEDGE_FILE),
    Content = jsx:encode(KnowledgeBase, [pretty]),
    file:write_file(?KNOWLEDGE_FILE, Content).

%%====================================================================
%% Utilities
%%====================================================================

ensure_timer_running(undefined) ->
    erlang:send_after(?CYCLE_INTERVAL, self(), run_cycle);
ensure_timer_running(Ref) ->
    case erlang:read_timer(Ref) of
        false -> erlang:send_after(?CYCLE_INTERVAL, self(), run_cycle);
        _ -> Ref
    end.
