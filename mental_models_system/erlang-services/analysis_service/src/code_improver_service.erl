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
%% Helper modules: code_improver_service_part2, code_improver_service_part3, code_improver_service_part4
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
