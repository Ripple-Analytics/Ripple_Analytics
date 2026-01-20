%%%-------------------------------------------------------------------
%%% @doc code_improver_service Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(code_improver_service_part2).

-export([handle_call/4, handle_call/3, handle_call/3, handle_cast/2, handle_info/2, handle_info/2, handle_info/2, handle_info/2, terminate/2, start_timer/1, stop_timer/1, stop_timer/1, get_target_modules/0, check_lm_studio/1, run_improvement_cycle/1, select_module_for_improvement/1]).

handle_call({set_config, Config}, _From, State) ->
    Interval = maps:get(interval, Config, State#state.interval),
    MaxDaily = maps:get(max_daily, Config, State#state.max_daily_improvements),
    NewState = State#state{
        interval = Interval,
        max_daily_improvements = MaxDaily
    },
    FinalState = case State#state.active of
        true -> start_timer(stop_timer(NewState));
        false -> NewState
    end,
    {reply, {ok, #{interval => Interval, max_daily => MaxDaily}}, FinalState};

handle_call(get_config, _From, State) ->
    Config = #{
        <<"interval_ms">> => State#state.interval,
        <<"max_daily_improvements">> => State#state.max_daily_improvements,
        <<"lm_studio_url">> => list_to_binary(State#state.lm_studio_url),
        <<"active">> => State#state.active
    },
    {reply, {ok, Config}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(run_improvement_cycle, State) when State#state.active ->
    NewState = case State#state.improvements_today < State#state.max_daily_improvements of
        true ->
            run_improvement_cycle(State);
        false ->
            io:format("[CodeImprover] Daily limit reached (~p), skipping~n", 
                      [State#state.max_daily_improvements]),
            State
    end,
    FinalState = start_timer(NewState),
    {noreply, FinalState};

handle_info(run_improvement_cycle, State) ->
    {noreply, State};

handle_info(reset_daily_count, State) ->
    io:format("[CodeImprover] Resetting daily improvement count~n"),
    erlang:send_after(86400000, self(), reset_daily_count),
    {noreply, State#state{improvements_today = 0}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    stop_timer(State),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

start_timer(State) ->
    NewState = stop_timer(State),
    Ref = erlang:send_after(State#state.interval, self(), run_improvement_cycle),
    NewState#state{timer_ref = Ref}.

stop_timer(#state{timer_ref = undefined} = State) ->
    State;
stop_timer(#state{timer_ref = Ref} = State) ->
    erlang:cancel_timer(Ref),
    State#state{timer_ref = undefined}.

get_target_modules() ->
    BaseDir = code:priv_dir(analysis_service),
    SrcDir = filename:join([filename:dirname(BaseDir), "src"]),
    case file:list_dir(SrcDir) of
        {ok, Files} ->
            [list_to_binary(F) || F <- Files, filename:extension(F) =:= ".erl"];
        {error, _} ->
            []
    end.

check_lm_studio(Url) ->
    HealthUrl = Url ++ "/v1/models",
    case hackney:request(get, list_to_binary(HealthUrl), [], <<>>, [{timeout, 5000}]) of
        {ok, 200, _, _} -> true;
        _ -> false
    end.

run_improvement_cycle(State) ->
    io:format("[CodeImprover] Running improvement cycle...~n"),
    
    case check_lm_studio(State#state.lm_studio_url) of
        false ->
            io:format("[CodeImprover] LM Studio not available, skipping~n"),
            State;
        true ->
            case select_module_for_improvement(State) of
                {ok, ModulePath} ->
                    case generate_improvement(ModulePath, State) of
                        {ok, Improvement} ->
                            case apply_and_validate(Improvement, State) of
                                {ok, _Result} ->
                                    io:format("[CodeImprover] Successfully applied improvement to ~s~n", 
                                              [ModulePath]),
                                    NewHistory = add_to_history(Improvement, success, State#state.history),
                                    notify_improvement(Improvement),
                                    State#state{
                                        history = NewHistory,
                                        total_improvements = State#state.total_improvements + 1,
                                        improvements_today = State#state.improvements_today + 1,
                                        last_improvement = calendar:local_time()
                                    };
                                {error, Reason} ->
                                    io:format("[CodeImprover] Failed to apply improvement: ~p~n", [Reason]),
                                    NewHistory = add_to_history(Improvement, {failed, Reason}, State#state.history),
                                    State#state{history = NewHistory}
                            end;
                        {error, Reason} ->
                            io:format("[CodeImprover] Failed to generate improvement: ~p~n", [Reason]),
                            State
                    end;
                {error, no_modules} ->
                    io:format("[CodeImprover] No modules available for improvement~n"),
                    State
            end
    end.

select_module_for_improvement(State) ->
    case State#state.target_modules of
        [] -> {error, no_modules};
        Modules ->
            Index = rand:uniform(length(Modules)),
            {ok, lists:nth(Index, Modules)}
    end.

