%%%-------------------------------------------------------------------
%%% @doc autonomous_engine Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(autonomous_engine_part2).

-export([handle_info/2, handle_info/2, handle_info/2, terminate/2, run_autonomous_cycle/1, analyze_codebase/0, analyze_file/1, has_issues/1, has_issues/1, has_issues/1, has_issues/1, has_issues/1, has_pattern/2]).

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

