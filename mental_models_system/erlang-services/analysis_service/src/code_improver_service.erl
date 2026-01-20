%%%-------------------------------------------------------------------
%%% @doc Code Improver Service - Autonomous Code Improvement
%%% 
%%% Orchestrates autonomous code improvement using LM Studio:
%%% - Periodically scans codebase for improvement opportunities
%%% - Generates improvements using local LLM
%%% - Validates generated code (syntax, compilation)
%%% - Deploys safely via blue-green deployment
%%% - Rolls back on failure
%%% 
%%% Runs 24/7 without supervision, following design philosophy.
%%% @end
%%%-------------------------------------------------------------------
-module(code_improver_service).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([start_improving/0, stop_improving/0, get_status/0]).
-export([suggest_improvement/1, apply_improvement/1, get_history/0]).
-export([set_config/1, get_config/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_INTERVAL, 300000). % 5 minutes
-define(MAX_IMPROVEMENTS_PER_CYCLE, 1).
-define(IMPROVEMENT_HISTORY_SIZE, 100).

-record(state, {
    active = false :: boolean(),
    interval = ?DEFAULT_INTERVAL :: pos_integer(),
    timer_ref = undefined :: undefined | reference(),
    lm_studio_url = "http://host.docker.internal:1234" :: string(),
    target_modules = [] :: [binary()],
    history = [] :: list(),
    improvements_today = 0 :: non_neg_integer(),
    max_daily_improvements = 10 :: non_neg_integer(),
    last_improvement = undefined :: undefined | calendar:datetime(),
    total_improvements = 0 :: non_neg_integer(),
    total_rollbacks = 0 :: non_neg_integer()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

start_improving() ->
    gen_server:call(?SERVER, start_improving).

stop_improving() ->
    gen_server:call(?SERVER, stop_improving).

get_status() ->
    gen_server:call(?SERVER, get_status).

suggest_improvement(ModulePath) ->
    gen_server:call(?SERVER, {suggest_improvement, ModulePath}, 120000).

apply_improvement(Improvement) ->
    gen_server:call(?SERVER, {apply_improvement, Improvement}, 120000).

get_history() ->
    gen_server:call(?SERVER, get_history).

set_config(Config) ->
    gen_server:call(?SERVER, {set_config, Config}).

get_config() ->
    gen_server:call(?SERVER, get_config).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Url = application:get_env(analysis_service, lm_studio_url, "http://host.docker.internal:1234"),
    TargetModules = get_target_modules(),
    
    io:format("[CodeImprover] Initialized. LM Studio: ~s~n", [Url]),
    io:format("[CodeImprover] Target modules: ~p~n", [length(TargetModules)]),
    
    {ok, #state{
        lm_studio_url = Url,
        target_modules = TargetModules
    }}.

handle_call(start_improving, _From, State) ->
    NewState = start_timer(State#state{active = true}),
    io:format("[CodeImprover] Started autonomous improvement cycle~n"),
    {reply, {ok, #{status => active}}, NewState};

handle_call(stop_improving, _From, State) ->
    NewState = stop_timer(State#state{active = false}),
    io:format("[CodeImprover] Stopped autonomous improvement cycle~n"),
    {reply, {ok, #{status => stopped}}, NewState};

handle_call(get_status, _From, State) ->
    Status = #{
        <<"active">> => State#state.active,
        <<"interval_ms">> => State#state.interval,
        <<"improvements_today">> => State#state.improvements_today,
        <<"max_daily">> => State#state.max_daily_improvements,
        <<"total_improvements">> => State#state.total_improvements,
        <<"total_rollbacks">> => State#state.total_rollbacks,
        <<"target_modules">> => length(State#state.target_modules),
        <<"last_improvement">> => format_datetime(State#state.last_improvement),
        <<"lm_studio_available">> => check_lm_studio(State#state.lm_studio_url)
    },
    {reply, {ok, Status}, State};

handle_call({suggest_improvement, ModulePath}, _From, State) ->
    Result = generate_improvement(ModulePath, State),
    {reply, Result, State};

handle_call({apply_improvement, Improvement}, _From, State) ->
    case apply_and_validate(Improvement, State) of
        {ok, Result} ->
            NewHistory = add_to_history(Improvement, success, State#state.history),
            NewState = State#state{
                history = NewHistory,
                total_improvements = State#state.total_improvements + 1,
                improvements_today = State#state.improvements_today + 1,
                last_improvement = calendar:local_time()
            },
            {reply, {ok, Result}, NewState};
        {error, Reason} ->
            NewHistory = add_to_history(Improvement, {failed, Reason}, State#state.history),
            {reply, {error, Reason}, State#state{history = NewHistory}}
    end;

handle_call(get_history, _From, State) ->
    {reply, {ok, State#state.history}, State};

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

generate_improvement(ModulePath, State) ->
    io:format("[CodeImprover] Generating improvement for ~s~n", [ModulePath]),
    
    BaseDir = code:priv_dir(analysis_service),
    SrcDir = filename:join([filename:dirname(BaseDir), "src"]),
    FullPath = filename:join(SrcDir, binary_to_list(ModulePath)),
    
    case file:read_file(FullPath) of
        {ok, CurrentCode} ->
            {SystemPrompt, UserPrompt} = prompt_templates:get_improvement_prompt(ModulePath, CurrentCode),
            
            case call_lm_studio(State#state.lm_studio_url, SystemPrompt, UserPrompt) of
                {ok, ImprovedCode} ->
                    {ok, #{
                        <<"module_path">> => ModulePath,
                        <<"full_path">> => list_to_binary(FullPath),
                        <<"original_code">> => CurrentCode,
                        <<"improved_code">> => ImprovedCode,
                        <<"generated_at">> => format_datetime(calendar:local_time())
                    }};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

call_lm_studio(Url, SystemPrompt, UserPrompt) ->
    ApiUrl = Url ++ "/v1/chat/completions",
    Headers = [{<<"content-type">>, <<"application/json">>}],
    
    RequestBody = jsx:encode(#{
        <<"model">> => <<"local-model">>,
        <<"messages">> => [
            #{<<"role">> => <<"system">>, <<"content">> => SystemPrompt},
            #{<<"role">> => <<"user">>, <<"content">> => UserPrompt}
        ],
        <<"max_tokens">> => 4000,
        <<"temperature">> => 0.3
    }),
    
    case hackney:request(post, list_to_binary(ApiUrl), Headers, RequestBody, [{timeout, 120000}]) of
        {ok, 200, _, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            Response = jsx:decode(RespBody, [return_maps]),
            Content = maps:get(<<"content">>, 
                        maps:get(<<"message">>, 
                            hd(maps:get(<<"choices">>, Response)))),
            {ok, extract_erlang_code(Content)};
        {ok, Status, _, _} ->
            {error, {http_error, Status}};
        {error, Reason} ->
            {error, Reason}
    end.

extract_erlang_code(Content) ->
    case binary:match(Content, <<"```erlang">>) of
        {Start, Len} ->
            AfterStart = binary:part(Content, Start + Len, byte_size(Content) - Start - Len),
            case binary:match(AfterStart, <<"```">>) of
                {End, _} ->
                    binary:part(AfterStart, 0, End);
                nomatch ->
                    AfterStart
            end;
        nomatch ->
            Content
    end.

apply_and_validate(Improvement, _State) ->
    FullPath = binary_to_list(maps:get(<<"full_path">>, Improvement)),
    ImprovedCode = maps:get(<<"improved_code">>, Improvement),
    OriginalCode = maps:get(<<"original_code">>, Improvement),
    
    case validate_erlang_syntax(ImprovedCode) of
        ok ->
            BackupPath = FullPath ++ ".backup",
            file:write_file(BackupPath, OriginalCode),
            
            case file:write_file(FullPath, ImprovedCode) of
                ok ->
                    case compile_module(FullPath) of
                        ok ->
                            file:delete(BackupPath),
                            {ok, #{
                                <<"status">> => <<"applied">>,
                                <<"path">> => list_to_binary(FullPath)
                            }};
                        {error, CompileError} ->
                            file:write_file(FullPath, OriginalCode),
                            file:delete(BackupPath),
                            {error, {compile_error, CompileError}}
                    end;
                {error, WriteError} ->
                    {error, {write_error, WriteError}}
            end;
        {error, SyntaxError} ->
            {error, {syntax_error, SyntaxError}}
    end.

validate_erlang_syntax(Code) ->
    TempFile = "/tmp/code_improver_temp.erl",
    file:write_file(TempFile, Code),
    case erl_scan:string(binary_to_list(Code)) of
        {ok, Tokens, _} ->
            case erl_parse:parse_form(Tokens) of
                {ok, _} -> ok;
                {error, _} ->
                    case compile:file(TempFile, [return_errors, binary]) of
                        {ok, _, _} -> ok;
                        {error, Errors, _} -> {error, Errors}
                    end
            end;
        {error, ErrorInfo, _} ->
            {error, ErrorInfo}
    end.

compile_module(FilePath) ->
    case compile:file(FilePath, [return_errors]) of
        {ok, _Module} -> ok;
        {ok, _Module, _Warnings} -> ok;
        {error, Errors, _Warnings} -> {error, Errors}
    end.

add_to_history(Improvement, Status, History) ->
    Entry = #{
        <<"module">> => maps:get(<<"module_path">>, Improvement),
        <<"status">> => format_status(Status),
        <<"timestamp">> => format_datetime(calendar:local_time())
    },
    lists:sublist([Entry | History], ?IMPROVEMENT_HISTORY_SIZE).

format_status(success) -> <<"success">>;
format_status({failed, Reason}) -> 
    list_to_binary(io_lib:format("failed: ~p", [Reason])).

notify_improvement(Improvement) ->
    try
        notification_service:notify(code_improvement, #{
            <<"module">> => maps:get(<<"module_path">>, Improvement),
            <<"message">> => <<"Code improvement applied successfully">>
        })
    catch
        _:_ -> ok
    end.

format_datetime(undefined) -> <<"never">>;
format_datetime({{Y, M, D}, {H, Mi, S}}) ->
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", 
                                  [Y, M, D, H, Mi, S])).
