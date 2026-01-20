%%%-------------------------------------------------------------------
%%% @doc Model Registry
%%% 
%%% Stores and manages mental models with their categories,
%%% descriptions, and failure modes.
%%% @end
%%%-------------------------------------------------------------------
-module(model_registry).
-behaviour(gen_server).

-export([start_link/0, get_all_models/0, get_model/1, get_categories/0, 
         search_models/1, get_models_by_category/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

-record(model, {
    id :: binary(),
    name :: binary(),
    category :: binary(),
    description :: binary(),
    key_insight :: binary(),
    application :: binary(),
    failure_modes :: [binary()],
    keywords :: [binary()]
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_all_models() ->
    gen_server:call(?SERVER, get_all).

get_model(Id) ->
    gen_server:call(?SERVER, {get, Id}).

get_categories() ->
    gen_server:call(?SERVER, get_categories).

search_models(Query) ->
    gen_server:call(?SERVER, {search, Query}).

get_models_by_category(Category) ->
    gen_server:call(?SERVER, {by_category, Category}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Initialize ETS table for models
    ets:new(models, [named_table, public, {keypos, #model.id}]),
    
    %% Load default mental models
    load_default_models(),
    
    {ok, #{}}.

handle_call(get_all, _From, State) ->
    Models = ets:tab2list(models),
    {reply, Models, State};

handle_call({get, Id}, _From, State) ->
    case ets:lookup(models, Id) of
        [Model] -> {reply, {ok, Model}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call(get_categories, _From, State) ->
    Models = ets:tab2list(models),
    Categories = lists:usort([M#model.category || M <- Models]),
    {reply, Categories, State};

handle_call({search, Query}, _From, State) ->
    QueryLower = string:lowercase(binary_to_list(Query)),
    Models = ets:tab2list(models),
    Matches = lists:filter(fun(M) ->
        NameLower = string:lowercase(binary_to_list(M#model.name)),
        DescLower = string:lowercase(binary_to_list(M#model.description)),
        string:find(NameLower, QueryLower) =/= nomatch orelse
        string:find(DescLower, QueryLower) =/= nomatch
    end, Models),
    {reply, Matches, State};

handle_call({by_category, Category}, _From, State) ->
    Models = ets:tab2list(models),
    Matches = [M || M <- Models, M#model.category =:= Category],
    {reply, Matches, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

load_default_models() ->
    Models = [
        #model{
            id = <<"inversion">>,
            name = <<"Inversion">>,
            category = <<"Thinking Tools">>,
            description = <<"Think backward from failure to avoid it. Instead of asking how to succeed, ask how to fail and avoid those paths.">>,
            key_insight = <<"Many problems are best solved by inverting them">>,
            application = <<"Pre-mortem analysis, risk assessment, goal setting">>,
            failure_modes = [<<"Paralysis by analysis">>, <<"Excessive pessimism">>],
            keywords = [<<"invert">>, <<"backward">>, <<"failure">>, <<"avoid">>]
        },
        #model{
            id = <<"circle_of_competence">>,
            name = <<"Circle of Competence">>,
            category = <<"Thinking Tools">>,
            description = <<"Know the boundaries of your expertise. Stay within your circle for important decisions.">>,
            key_insight = <<"Knowing what you don't know is more valuable than what you know">>,
            application = <<"Investment decisions, career choices, delegation">>,
            failure_modes = [<<"Overconfidence">>, <<"Missed opportunities">>],
            keywords = [<<"competence">>, <<"expertise">>, <<"knowledge">>, <<"boundaries">>]
        },
        #model{
            id = <<"first_principles">>,
            name = <<"First Principles Thinking">>,
            category = <<"Thinking Tools">>,
            description = <<"Break down complex problems into fundamental truths and reason up from there.">>,
            key_insight = <<"Most constraints are assumed, not real">>,
            application = <<"Innovation, problem-solving, challenging assumptions">>,
            failure_modes = [<<"Reinventing the wheel">>, <<"Analysis paralysis">>],
            keywords = [<<"fundamental">>, <<"basic">>, <<"truth">>, <<"reasoning">>]
        },
        #model{
            id = <<"second_order_thinking">>,
            name = <<"Second-Order Thinking">>,
            category = <<"Thinking Tools">>,
            description = <<"Consider the consequences of consequences. Think beyond immediate effects.">>,
            key_insight = <<"First-level thinking is simplistic; second-level thinking is deep">>,
            application = <<"Policy decisions, strategy, long-term planning">>,
            failure_modes = [<<"Overthinking">>, <<"Delayed action">>],
            keywords = [<<"consequences">>, <<"effects">>, <<"long-term">>, <<"ripple">>]
        },
        #model{
            id = <<"confirmation_bias">>,
            name = <<"Confirmation Bias">>,
            category = <<"Psychology: Biases">>,
            description = <<"The tendency to search for, interpret, and recall information that confirms pre-existing beliefs.">>,
            key_insight = <<"We see what we want to see">>,
            application = <<"Research, decision-making, self-awareness">>,
            failure_modes = [<<"Echo chambers">>, <<"Poor decisions">>],
            keywords = [<<"bias">>, <<"confirm">>, <<"belief">>, <<"evidence">>]
        },
        #model{
            id = <<"loss_aversion">>,
            name = <<"Loss Aversion">>,
            category = <<"Psychology: Biases">>,
            description = <<"Losses feel roughly twice as painful as equivalent gains feel good.">>,
            key_insight = <<"Fear of loss often outweighs desire for gain">>,
            application = <<"Negotiation, pricing, risk management">>,
            failure_modes = [<<"Holding losers too long">>, <<"Avoiding necessary risks">>],
            keywords = [<<"loss">>, <<"pain">>, <<"gain">>, <<"fear">>]
        },
        #model{
            id = <<"availability_heuristic">>,
            name = <<"Availability Heuristic">>,
            category = <<"Psychology: Biases">>,
            description = <<"Judging probability by how easily examples come to mind.">>,
            key_insight = <<"Vivid events seem more likely than they are">>,
            application = <<"Risk assessment, media literacy, decision-making">>,
            failure_modes = [<<"Overreacting to news">>, <<"Underestimating quiet risks">>],
            keywords = [<<"available">>, <<"memory">>, <<"vivid">>, <<"probability">>]
        },
        #model{
            id = <<"margin_of_safety">>,
            name = <<"Margin of Safety">>,
            category = <<"Economics & Finance">>,
            description = <<"Build in a buffer for errors, unknowns, and bad luck.">>,
            key_insight = <<"The future is uncertain; plan accordingly">>,
            application = <<"Investing, engineering, project planning">>,
            failure_modes = [<<"Over-conservatism">>, <<"Missed opportunities">>],
            keywords = [<<"safety">>, <<"buffer">>, <<"margin">>, <<"conservative">>]
        },
        #model{
            id = <<"compound_interest">>,
            name = <<"Compound Interest">>,
            category = <<"Economics & Finance">>,
            description = <<"Small gains accumulate exponentially over time.">>,
            key_insight = <<"Time is the most powerful force in compounding">>,
            application = <<"Investing, learning, habit formation">>,
            failure_modes = [<<"Impatience">>, <<"Underestimating long-term effects">>],
            keywords = [<<"compound">>, <<"exponential">>, <<"growth">>, <<"time">>]
        },
        #model{
            id = <<"opportunity_cost">>,
            name = <<"Opportunity Cost">>,
            category = <<"Economics & Finance">>,
            description = <<"The cost of any choice is the value of the best alternative foregone.">>,
            key_insight = <<"Every yes is a no to something else">>,
            application = <<"Resource allocation, time management, strategy">>,
            failure_modes = [<<"Paralysis">>, <<"Regret">>],
            keywords = [<<"opportunity">>, <<"cost">>, <<"alternative">>, <<"tradeoff">>]
        },
        #model{
            id = <<"feedback_loops">>,
            name = <<"Feedback Loops">>,
            category = <<"Systems Thinking">>,
            description = <<"Outputs of a system become inputs, creating reinforcing or balancing cycles.">>,
            key_insight = <<"Small changes can have large effects through feedback">>,
            application = <<"System design, habit formation, market analysis">>,
            failure_modes = [<<"Runaway effects">>, <<"Oscillation">>],
            keywords = [<<"feedback">>, <<"loop">>, <<"cycle">>, <<"reinforcing">>]
        },
        #model{
            id = <<"emergence">>,
            name = <<"Emergence">>,
            category = <<"Systems Thinking">>,
            description = <<"Complex behaviors arise from simple rules and interactions.">>,
            key_insight = <<"The whole is greater than the sum of its parts">>,
            application = <<"Organization design, understanding markets, AI">>,
            failure_modes = [<<"Reductionism">>, <<"Missing emergent properties">>],
            keywords = [<<"emerge">>, <<"complex">>, <<"simple">>, <<"whole">>]
        }
    ],
    
    lists:foreach(fun(Model) ->
        ets:insert(models, Model)
    end, Models).
