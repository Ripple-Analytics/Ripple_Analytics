%%%-------------------------------------------------------------------
%%% @doc UI Customizer - Autonomous UI improvement and customization
%%% 
%%% Enables the system to modify its own UI:
%%% - Analyze UI handler files
%%% - Generate improved layouts and components
%%% - Add new features and pages
%%% - Optimize information density
%%% - Track changes and rollback if needed
%%% @end
%%%-------------------------------------------------------------------
-module(ui_customizer).
%% Helper modules: ui_customizer_part2, ui_customizer_part3, ui_customizer_part4
-behaviour(gen_server).

-export([start_link/0, get_status/0, analyze_ui/0, improve_handler/1,
         add_feature/2, generate_page/2, optimize_layout/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    ui_analysis = #{} :: map(),
    pending_changes = [] :: list(),
    applied_changes = [] :: list(),
    rollback_stack = [] :: list(),
    total_improvements = 0 :: integer()
}).

-define(UI_PATH, "/repo/mental_models_system/erlang-services/desktop_ui/src").

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_status() ->
    gen_server:call(?MODULE, get_status).

analyze_ui() ->
    gen_server:call(?MODULE, analyze_ui, 60000).

improve_handler(HandlerName) ->
    gen_server:call(?MODULE, {improve_handler, HandlerName}, 120000).

add_feature(HandlerName, FeatureSpec) ->
    gen_server:call(?MODULE, {add_feature, HandlerName, FeatureSpec}, 120000).

generate_page(PageName, PageSpec) ->
    gen_server:call(?MODULE, {generate_page, PageName, PageSpec}, 120000).

optimize_layout(HandlerName) ->
    gen_server:call(?MODULE, {optimize_layout, HandlerName}, 120000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[UI-CUSTOMIZER] UI Customizer initialized~n"),
    {ok, #state{}}.

handle_call(get_status, _From, State) ->
    Status = #{
        <<"ui_handlers">> => maps:keys(State#state.ui_analysis),
        <<"pending_changes">> => length(State#state.pending_changes),
        <<"applied_changes">> => length(State#state.applied_changes),
        <<"total_improvements">> => State#state.total_improvements
    },
    {reply, {ok, Status}, State};

handle_call(analyze_ui, _From, State) ->
    io:format("[UI-CUSTOMIZER] Analyzing UI handlers...~n"),
    Analysis = do_analyze_ui(),
    {reply, {ok, Analysis}, State#state{ui_analysis = Analysis}};

handle_call({improve_handler, HandlerName}, _From, State) ->
    io:format("[UI-CUSTOMIZER] Improving handler: ~s~n", [HandlerName]),
    Result = do_improve_handler(HandlerName, State#state.ui_analysis),
    NewState = case Result of
        {ok, Change} ->
            State#state{
                applied_changes = [Change | State#state.applied_changes],
                total_improvements = State#state.total_improvements + 1
            };
        _ -> State
    end,
    {reply, Result, NewState};

handle_call({add_feature, HandlerName, FeatureSpec}, _From, State) ->
    io:format("[UI-CUSTOMIZER] Adding feature to ~s: ~p~n", [HandlerName, FeatureSpec]),
    Result = do_add_feature(HandlerName, FeatureSpec),
    NewState = case Result of
        {ok, Change} ->
            State#state{
                applied_changes = [Change | State#state.applied_changes],
                total_improvements = State#state.total_improvements + 1
            };
        _ -> State
    end,
    {reply, Result, NewState};

handle_call({generate_page, PageName, PageSpec}, _From, State) ->
    io:format("[UI-CUSTOMIZER] Generating new page: ~s~n", [PageName]),
    Result = do_generate_page(PageName, PageSpec),
    NewState = case Result of
        {ok, Change} ->
            State#state{
                applied_changes = [Change | State#state.applied_changes],
                total_improvements = State#state.total_improvements + 1
            };
        _ -> State
    end,
    {reply, Result, NewState};

handle_call({optimize_layout, HandlerName}, _From, State) ->
    io:format("[UI-CUSTOMIZER] Optimizing layout for: ~s~n", [HandlerName]),
    Result = do_optimize_layout(HandlerName),
    NewState = case Result of
        {ok, Change} ->
            State#state{
                applied_changes = [Change | State#state.applied_changes],
                total_improvements = State#state.total_improvements + 1
            };
        _ -> State
    end,
    {reply, Result, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% UI Analysis
%%====================================================================
