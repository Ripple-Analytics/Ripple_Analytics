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
    %% Convert records to maps for easier consumption by other modules
    ModelMaps = lists:map(fun(M) -> model_to_map(M) end, Models),
    {reply, ModelMaps, State};

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

%% @doc Convert a model record to a map for easier consumption
model_to_map(#model{id = Id, name = Name, category = Category, 
                    description = Description, key_insight = KeyInsight,
                    application = Application, failure_modes = FailureModes,
                    keywords = Keywords}) ->
    #{
        <<"id">> => Id,
        <<"name">> => Name,
        <<"category">> => Category,
        <<"description">> => Description,
        <<"key_insight">> => KeyInsight,
        <<"application">> => Application,
        <<"failure_modes">> => FailureModes,
        <<"keywords">> => Keywords
    }.

load_default_models() ->
    Models = [
        %% ============================================
        %% THINKING TOOLS (Core Decision-Making Models)
        %% ============================================
        #model{
            id = <<"inversion">>,
            name = <<"Inversion">>,
            category = <<"Thinking Tools">>,
            description = <<"Think backward from failure to avoid it. Instead of asking how to succeed, ask how to fail and avoid those paths.">>,
            key_insight = <<"Many problems are best solved by inverting them">>,
            application = <<"Pre-mortem analysis, risk assessment, goal setting">>,
            failure_modes = [<<"Paralysis by analysis">>, <<"Excessive pessimism">>],
            keywords = [<<"invert">>, <<"backward">>, <<"failure">>, <<"avoid">>, <<"reverse">>]
        },
        #model{
            id = <<"circle_of_competence">>,
            name = <<"Circle of Competence">>,
            category = <<"Thinking Tools">>,
            description = <<"Know the boundaries of your expertise. Stay within your circle for important decisions.">>,
            key_insight = <<"Knowing what you don't know is more valuable than what you know">>,
            application = <<"Investment decisions, career choices, delegation">>,
            failure_modes = [<<"Overconfidence">>, <<"Missed opportunities">>],
            keywords = [<<"competence">>, <<"expertise">>, <<"knowledge">>, <<"boundaries">>, <<"specialist">>]
        },
        #model{
            id = <<"first_principles">>,
            name = <<"First Principles Thinking">>,
            category = <<"Thinking Tools">>,
            description = <<"Break down complex problems into fundamental truths and reason up from there.">>,
            key_insight = <<"Most constraints are assumed, not real">>,
            application = <<"Innovation, problem-solving, challenging assumptions">>,
            failure_modes = [<<"Reinventing the wheel">>, <<"Analysis paralysis">>],
            keywords = [<<"fundamental">>, <<"basic">>, <<"truth">>, <<"reasoning">>, <<"axiom">>]
        },
        #model{
            id = <<"second_order_thinking">>,
            name = <<"Second-Order Thinking">>,
            category = <<"Thinking Tools">>,
            description = <<"Consider the consequences of consequences. Think beyond immediate effects.">>,
            key_insight = <<"First-level thinking is simplistic; second-level thinking is deep">>,
            application = <<"Policy decisions, strategy, long-term planning">>,
            failure_modes = [<<"Overthinking">>, <<"Delayed action">>],
            keywords = [<<"consequences">>, <<"effects">>, <<"long-term">>, <<"ripple">>, <<"cascade">>]
        },
        #model{
            id = <<"occams_razor">>,
            name = <<"Occam's Razor">>,
            category = <<"Thinking Tools">>,
            description = <<"The simplest explanation is usually correct. Don't multiply entities beyond necessity.">>,
            key_insight = <<"Complexity should not be assumed without evidence">>,
            application = <<"Debugging, hypothesis formation, decision-making">>,
            failure_modes = [<<"Oversimplification">>, <<"Missing nuance">>],
            keywords = [<<"simple">>, <<"explanation">>, <<"parsimony">>, <<"complexity">>]
        },
        #model{
            id = <<"hanlons_razor">>,
            name = <<"Hanlon's Razor">>,
            category = <<"Thinking Tools">>,
            description = <<"Never attribute to malice that which can be adequately explained by stupidity or incompetence.">>,
            key_insight = <<"Most mistakes are honest errors, not conspiracies">>,
            application = <<"Conflict resolution, team dynamics, customer service">>,
            failure_modes = [<<"Naivety">>, <<"Missing actual malice">>],
            keywords = [<<"malice">>, <<"stupidity">>, <<"incompetence">>, <<"mistake">>]
        },
        #model{
            id = <<"probabilistic_thinking">>,
            name = <<"Probabilistic Thinking">>,
            category = <<"Thinking Tools">>,
            description = <<"Think in probabilities rather than certainties. Assign likelihoods to outcomes.">>,
            key_insight = <<"The future is a distribution of possibilities, not a single path">>,
            application = <<"Risk assessment, forecasting, decision-making under uncertainty">>,
            failure_modes = [<<"False precision">>, <<"Ignoring base rates">>],
            keywords = [<<"probability">>, <<"likelihood">>, <<"odds">>, <<"chance">>, <<"uncertainty">>]
        },
        
        %% ============================================
        %% PSYCHOLOGY: BIASES & TENDENCIES
        %% ============================================
        #model{
            id = <<"confirmation_bias">>,
            name = <<"Confirmation Bias">>,
            category = <<"Psychology: Biases">>,
            description = <<"The tendency to search for, interpret, and recall information that confirms pre-existing beliefs.">>,
            key_insight = <<"We see what we want to see">>,
            application = <<"Research, decision-making, self-awareness">>,
            failure_modes = [<<"Echo chambers">>, <<"Poor decisions">>],
            keywords = [<<"bias">>, <<"confirm">>, <<"belief">>, <<"evidence">>, <<"cherry-pick">>]
        },
        #model{
            id = <<"loss_aversion">>,
            name = <<"Loss Aversion">>,
            category = <<"Psychology: Biases">>,
            description = <<"Losses feel roughly twice as painful as equivalent gains feel good.">>,
            key_insight = <<"Fear of loss often outweighs desire for gain">>,
            application = <<"Negotiation, pricing, risk management">>,
            failure_modes = [<<"Holding losers too long">>, <<"Avoiding necessary risks">>],
            keywords = [<<"loss">>, <<"pain">>, <<"gain">>, <<"fear">>, <<"aversion">>]
        },
        #model{
            id = <<"availability_heuristic">>,
            name = <<"Availability Heuristic">>,
            category = <<"Psychology: Biases">>,
            description = <<"Judging probability by how easily examples come to mind.">>,
            key_insight = <<"Vivid events seem more likely than they are">>,
            application = <<"Risk assessment, media literacy, decision-making">>,
            failure_modes = [<<"Overreacting to news">>, <<"Underestimating quiet risks">>],
            keywords = [<<"available">>, <<"memory">>, <<"vivid">>, <<"probability">>, <<"recall">>]
        },
        #model{
            id = <<"anchoring">>,
            name = <<"Anchoring">>,
            category = <<"Psychology: Biases">>,
            description = <<"Over-relying on the first piece of information encountered when making decisions.">>,
            key_insight = <<"First impressions create reference points that bias subsequent judgments">>,
            application = <<"Negotiation, pricing, estimation">>,
            failure_modes = [<<"Arbitrary anchors">>, <<"Insufficient adjustment">>],
            keywords = [<<"anchor">>, <<"first">>, <<"initial">>, <<"reference">>, <<"starting">>]
        },
        #model{
            id = <<"hindsight_bias">>,
            name = <<"Hindsight Bias">>,
            category = <<"Psychology: Biases">>,
            description = <<"The tendency to see past events as having been predictable, even when they weren't.">>,
            key_insight = <<"Knowing the outcome makes it seem inevitable">>,
            application = <<"Post-mortems, learning from mistakes, historical analysis">>,
            failure_modes = [<<"Overconfidence in predictions">>, <<"Unfair blame">>],
            keywords = [<<"hindsight">>, <<"obvious">>, <<"predictable">>, <<"knew it">>]
        },
        #model{
            id = <<"sunk_cost_fallacy">>,
            name = <<"Sunk Cost Fallacy">>,
            category = <<"Psychology: Biases">>,
            description = <<"Continuing a behavior due to previously invested resources rather than future value.">>,
            key_insight = <<"Past costs are irrelevant to future decisions">>,
            application = <<"Project management, investing, relationships">>,
            failure_modes = [<<"Throwing good money after bad">>, <<"Escalation of commitment">>],
            keywords = [<<"sunk">>, <<"cost">>, <<"invested">>, <<"already">>, <<"committed">>]
        },
        #model{
            id = <<"dunning_kruger">>,
            name = <<"Dunning-Kruger Effect">>,
            category = <<"Psychology: Biases">>,
            description = <<"Unskilled individuals overestimate their ability; experts underestimate theirs.">>,
            key_insight = <<"Competence breeds humility; incompetence breeds confidence">>,
            application = <<"Self-assessment, hiring, team building">>,
            failure_modes = [<<"Imposter syndrome">>, <<"Overconfident novices">>],
            keywords = [<<"overconfident">>, <<"incompetent">>, <<"expert">>, <<"humble">>]
        },
        #model{
            id = <<"social_proof">>,
            name = <<"Social Proof">>,
            category = <<"Psychology: Biases">>,
            description = <<"Looking to others' behavior to determine correct action, especially under uncertainty.">>,
            key_insight = <<"We assume the crowd knows something we don't">>,
            application = <<"Marketing, leadership, crisis management">>,
            failure_modes = [<<"Herd behavior">>, <<"Pluralistic ignorance">>],
            keywords = [<<"social">>, <<"proof">>, <<"crowd">>, <<"others">>, <<"popular">>]
        },
        #model{
            id = <<"recency_bias">>,
            name = <<"Recency Bias">>,
            category = <<"Psychology: Biases">>,
            description = <<"Giving more weight to recent events than earlier ones.">>,
            key_insight = <<"What happened last feels most important">>,
            application = <<"Performance reviews, investing, forecasting">>,
            failure_modes = [<<"Ignoring long-term trends">>, <<"Overreacting to recent events">>],
            keywords = [<<"recent">>, <<"latest">>, <<"last">>, <<"new">>, <<"current">>]
        },
        #model{
            id = <<"status_quo_bias">>,
            name = <<"Status Quo Bias">>,
            category = <<"Psychology: Biases">>,
            description = <<"Preference for the current state of affairs over change.">>,
            key_insight = <<"Change feels risky even when staying put is riskier">>,
            application = <<"Change management, product design, policy">>,
            failure_modes = [<<"Missed opportunities">>, <<"Stagnation">>],
            keywords = [<<"status quo">>, <<"current">>, <<"change">>, <<"default">>, <<"existing">>]
        },
        
        %% ============================================
        %% ECONOMICS & FINANCE
        %% ============================================
        #model{
            id = <<"margin_of_safety">>,
            name = <<"Margin of Safety">>,
            category = <<"Economics & Finance">>,
            description = <<"Build in a buffer for errors, unknowns, and bad luck.">>,
            key_insight = <<"The future is uncertain; plan accordingly">>,
            application = <<"Investing, engineering, project planning">>,
            failure_modes = [<<"Over-conservatism">>, <<"Missed opportunities">>],
            keywords = [<<"safety">>, <<"buffer">>, <<"margin">>, <<"conservative">>, <<"cushion">>]
        },
        #model{
            id = <<"compound_interest">>,
            name = <<"Compound Interest">>,
            category = <<"Economics & Finance">>,
            description = <<"Small gains accumulate exponentially over time.">>,
            key_insight = <<"Time is the most powerful force in compounding">>,
            application = <<"Investing, learning, habit formation">>,
            failure_modes = [<<"Impatience">>, <<"Underestimating long-term effects">>],
            keywords = [<<"compound">>, <<"exponential">>, <<"growth">>, <<"time">>, <<"accumulate">>]
        },
        #model{
            id = <<"opportunity_cost">>,
            name = <<"Opportunity Cost">>,
            category = <<"Economics & Finance">>,
            description = <<"The cost of any choice is the value of the best alternative foregone.">>,
            key_insight = <<"Every yes is a no to something else">>,
            application = <<"Resource allocation, time management, strategy">>,
            failure_modes = [<<"Paralysis">>, <<"Regret">>],
            keywords = [<<"opportunity">>, <<"cost">>, <<"alternative">>, <<"tradeoff">>, <<"foregone">>]
        },
        #model{
            id = <<"supply_demand">>,
            name = <<"Supply and Demand">>,
            category = <<"Economics & Finance">>,
            description = <<"Prices are determined by the interaction of supply and demand.">>,
            key_insight = <<"Scarcity drives value; abundance reduces it">>,
            application = <<"Pricing, market analysis, negotiation">>,
            failure_modes = [<<"Ignoring elasticity">>, <<"Missing market dynamics">>],
            keywords = [<<"supply">>, <<"demand">>, <<"price">>, <<"scarcity">>, <<"market">>]
        },
        #model{
            id = <<"incentives">>,
            name = <<"Incentives">>,
            category = <<"Economics & Finance">>,
            description = <<"People respond to incentives. Behavior follows rewards and punishments.">>,
            key_insight = <<"Show me the incentive and I'll show you the outcome">>,
            application = <<"Compensation design, policy, behavior change">>,
            failure_modes = [<<"Perverse incentives">>, <<"Gaming the system">>],
            keywords = [<<"incentive">>, <<"reward">>, <<"punishment">>, <<"motivation">>, <<"behavior">>]
        },
        #model{
            id = <<"comparative_advantage">>,
            name = <<"Comparative Advantage">>,
            category = <<"Economics & Finance">>,
            description = <<"Focus on what you do relatively better, even if not absolutely better.">>,
            key_insight = <<"Trade benefits both parties when each specializes">>,
            application = <<"Career decisions, team allocation, outsourcing">>,
            failure_modes = [<<"Ignoring absolute advantage">>, <<"Over-specialization">>],
            keywords = [<<"comparative">>, <<"advantage">>, <<"specialize">>, <<"trade">>, <<"relative">>]
        },
        #model{
            id = <<"diminishing_returns">>,
            name = <<"Diminishing Returns">>,
            category = <<"Economics & Finance">>,
            description = <<"Each additional unit of input yields progressively smaller output gains.">>,
            key_insight = <<"More isn't always better; know when to stop">>,
            application = <<"Resource allocation, optimization, effort management">>,
            failure_modes = [<<"Over-investment">>, <<"Perfectionism">>],
            keywords = [<<"diminishing">>, <<"returns">>, <<"marginal">>, <<"additional">>, <<"less">>]
        },
        #model{
            id = <<"network_effects">>,
            name = <<"Network Effects">>,
            category = <<"Economics & Finance">>,
            description = <<"A product becomes more valuable as more people use it.">>,
            key_insight = <<"Winner-take-all dynamics emerge from network effects">>,
            application = <<"Platform strategy, market analysis, competitive moats">>,
            failure_modes = [<<"Overestimating network strength">>, <<"Ignoring multi-homing">>],
            keywords = [<<"network">>, <<"effect">>, <<"users">>, <<"platform">>, <<"viral">>]
        },
        
        %% ============================================
        %% SYSTEMS THINKING
        %% ============================================
        #model{
            id = <<"feedback_loops">>,
            name = <<"Feedback Loops">>,
            category = <<"Systems Thinking">>,
            description = <<"Outputs of a system become inputs, creating reinforcing or balancing cycles.">>,
            key_insight = <<"Small changes can have large effects through feedback">>,
            application = <<"System design, habit formation, market analysis">>,
            failure_modes = [<<"Runaway effects">>, <<"Oscillation">>],
            keywords = [<<"feedback">>, <<"loop">>, <<"cycle">>, <<"reinforcing">>, <<"balancing">>]
        },
        #model{
            id = <<"emergence">>,
            name = <<"Emergence">>,
            category = <<"Systems Thinking">>,
            description = <<"Complex behaviors arise from simple rules and interactions.">>,
            key_insight = <<"The whole is greater than the sum of its parts">>,
            application = <<"Organization design, understanding markets, AI">>,
            failure_modes = [<<"Reductionism">>, <<"Missing emergent properties">>],
            keywords = [<<"emerge">>, <<"complex">>, <<"simple">>, <<"whole">>, <<"parts">>]
        },
        #model{
            id = <<"bottlenecks">>,
            name = <<"Bottlenecks">>,
            category = <<"Systems Thinking">>,
            description = <<"System throughput is limited by its narrowest constraint.">>,
            key_insight = <<"Improving non-bottlenecks doesn't improve the system">>,
            application = <<"Process optimization, capacity planning, debugging">>,
            failure_modes = [<<"Optimizing wrong areas">>, <<"Creating new bottlenecks">>],
            keywords = [<<"bottleneck">>, <<"constraint">>, <<"limit">>, <<"throughput">>, <<"capacity">>]
        },
        #model{
            id = <<"leverage_points">>,
            name = <<"Leverage Points">>,
            category = <<"Systems Thinking">>,
            description = <<"Places in a system where small changes can produce large effects.">>,
            key_insight = <<"Not all interventions are equal; find the leverage">>,
            application = <<"Change management, policy design, problem-solving">>,
            failure_modes = [<<"Pushing in wrong direction">>, <<"Missing higher leverage points">>],
            keywords = [<<"leverage">>, <<"point">>, <<"intervention">>, <<"change">>, <<"impact">>]
        },
        #model{
            id = <<"redundancy">>,
            name = <<"Redundancy">>,
            category = <<"Systems Thinking">>,
            description = <<"Backup systems and components that prevent single points of failure.">>,
            key_insight = <<"Reliability requires redundancy; efficiency opposes it">>,
            application = <<"System design, risk management, planning">>,
            failure_modes = [<<"Over-engineering">>, <<"Complacency">>],
            keywords = [<<"redundancy">>, <<"backup">>, <<"failover">>, <<"resilience">>, <<"spare">>]
        },
        
        %% ============================================
        %% PHYSICS & ENGINEERING
        %% ============================================
        #model{
            id = <<"entropy">>,
            name = <<"Entropy">>,
            category = <<"Physics">>,
            description = <<"Systems naturally tend toward disorder without energy input.">>,
            key_insight = <<"Maintenance is not optional; decay is the default">>,
            application = <<"Project management, relationships, organizations">>,
            failure_modes = [<<"Neglecting maintenance">>, <<"Expecting permanence">>],
            keywords = [<<"entropy">>, <<"disorder">>, <<"decay">>, <<"maintenance">>, <<"chaos">>]
        },
        #model{
            id = <<"inertia">>,
            name = <<"Inertia">>,
            category = <<"Physics">>,
            description = <<"Objects in motion stay in motion; objects at rest stay at rest.">>,
            key_insight = <<"Starting is hard; momentum makes continuation easier">>,
            application = <<"Habit formation, change management, project initiation">>,
            failure_modes = [<<"Difficulty starting">>, <<"Difficulty stopping">>],
            keywords = [<<"inertia">>, <<"momentum">>, <<"motion">>, <<"rest">>, <<"resistance">>]
        },
        #model{
            id = <<"critical_mass">>,
            name = <<"Critical Mass">>,
            category = <<"Physics">>,
            description = <<"The minimum amount needed to sustain a chain reaction.">>,
            key_insight = <<"Below threshold, nothing happens; above it, everything changes">>,
            application = <<"Product launches, social movements, viral growth">>,
            failure_modes = [<<"Giving up too early">>, <<"Overestimating threshold">>],
            keywords = [<<"critical">>, <<"mass">>, <<"threshold">>, <<"tipping">>, <<"point">>]
        },
        #model{
            id = <<"activation_energy">>,
            name = <<"Activation Energy">>,
            category = <<"Physics">>,
            description = <<"The initial energy required to start a reaction.">>,
            key_insight = <<"High barriers prevent action even when outcomes are favorable">>,
            application = <<"Behavior design, onboarding, habit formation">>,
            failure_modes = [<<"Barriers too high">>, <<"Ignoring friction">>],
            keywords = [<<"activation">>, <<"energy">>, <<"barrier">>, <<"start">>, <<"friction">>]
        },
        
        %% ============================================
        %% BIOLOGY & EVOLUTION
        %% ============================================
        #model{
            id = <<"natural_selection">>,
            name = <<"Natural Selection">>,
            category = <<"Biology">>,
            description = <<"Traits that improve survival and reproduction become more common over time.">>,
            key_insight = <<"What survives isn't always what's best, just what's fit enough">>,
            application = <<"Market competition, product evolution, organizational change">>,
            failure_modes = [<<"Survival bias">>, <<"Local optima">>],
            keywords = [<<"selection">>, <<"evolution">>, <<"survival">>, <<"adaptation">>, <<"fitness">>]
        },
        #model{
            id = <<"adaptation">>,
            name = <<"Adaptation">>,
            category = <<"Biology">>,
            description = <<"Organisms change to better fit their environment over time.">>,
            key_insight = <<"Flexibility beats strength in changing environments">>,
            application = <<"Strategy, career planning, organizational design">>,
            failure_modes = [<<"Over-adaptation">>, <<"Inability to change">>],
            keywords = [<<"adapt">>, <<"change">>, <<"environment">>, <<"flexible">>, <<"evolve">>]
        },
        #model{
            id = <<"red_queen_effect">>,
            name = <<"Red Queen Effect">>,
            category = <<"Biology">>,
            description = <<"You must keep running just to stay in place relative to competitors.">>,
            key_insight = <<"Standing still means falling behind">>,
            application = <<"Competitive strategy, innovation, skill development">>,
            failure_modes = [<<"Exhaustion">>, <<"Arms race">>],
            keywords = [<<"red queen">>, <<"competition">>, <<"running">>, <<"relative">>, <<"race">>]
        },
        #model{
            id = <<"ecosystems">>,
            name = <<"Ecosystems">>,
            category = <<"Biology">>,
            description = <<"Interconnected communities where changes ripple through relationships.">>,
            key_insight = <<"Everything is connected; interventions have side effects">>,
            application = <<"Platform strategy, market analysis, environmental policy">>,
            failure_modes = [<<"Ignoring dependencies">>, <<"Unintended consequences">>],
            keywords = [<<"ecosystem">>, <<"interconnected">>, <<"community">>, <<"relationship">>, <<"web">>]
        },
        
        %% ============================================
        %% MATHEMATICS & STATISTICS
        %% ============================================
        #model{
            id = <<"regression_to_mean">>,
            name = <<"Regression to the Mean">>,
            category = <<"Mathematics">>,
            description = <<"Extreme outcomes tend to be followed by more average ones.">>,
            key_insight = <<"Exceptional performance often reverts to normal">>,
            application = <<"Performance evaluation, investing, sports">>,
            failure_modes = [<<"Attributing luck to skill">>, <<"Overreacting to outliers">>],
            keywords = [<<"regression">>, <<"mean">>, <<"average">>, <<"extreme">>, <<"revert">>]
        },
        #model{
            id = <<"power_laws">>,
            name = <<"Power Laws">>,
            category = <<"Mathematics">>,
            description = <<"A few items account for most of the effect (80/20 rule).">>,
            key_insight = <<"Focus on the vital few, not the trivial many">>,
            application = <<"Prioritization, resource allocation, analysis">>,
            failure_modes = [<<"Ignoring the long tail">>, <<"Over-concentration">>],
            keywords = [<<"power law">>, <<"pareto">>, <<"80/20">>, <<"distribution">>, <<"few">>]
        },
        #model{
            id = <<"bayes_theorem">>,
            name = <<"Bayes' Theorem">>,
            category = <<"Mathematics">>,
            description = <<"Update beliefs based on new evidence, weighted by prior probability.">>,
            key_insight = <<"Prior beliefs matter; evidence updates them proportionally">>,
            application = <<"Decision-making, diagnosis, forecasting">>,
            failure_modes = [<<"Ignoring base rates">>, <<"Overweighting new evidence">>],
            keywords = [<<"bayes">>, <<"prior">>, <<"posterior">>, <<"update">>, <<"evidence">>]
        },
        #model{
            id = <<"law_of_large_numbers">>,
            name = <<"Law of Large Numbers">>,
            category = <<"Mathematics">>,
            description = <<"As sample size increases, results converge to the expected value.">>,
            key_insight = <<"Small samples are unreliable; large samples reveal truth">>,
            application = <<"Research, A/B testing, risk management">>,
            failure_modes = [<<"Small sample fallacy">>, <<"Gambler's fallacy">>],
            keywords = [<<"large numbers">>, <<"sample">>, <<"average">>, <<"converge">>, <<"expected">>]
        },
        
        %% ============================================
        %% ORGANIZATIONAL & MOATS
        %% ============================================
        #model{
            id = <<"economies_of_scale">>,
            name = <<"Economies of Scale">>,
            category = <<"Organizational">>,
            description = <<"Cost per unit decreases as production volume increases.">>,
            key_insight = <<"Size creates cost advantages that compound">>,
            application = <<"Business strategy, pricing, competitive analysis">>,
            failure_modes = [<<"Diseconomies of scale">>, <<"Bureaucracy">>],
            keywords = [<<"scale">>, <<"economies">>, <<"volume">>, <<"cost">>, <<"size">>]
        },
        #model{
            id = <<"switching_costs">>,
            name = <<"Switching Costs">>,
            category = <<"Organizational">>,
            description = <<"The costs (time, money, effort) of changing from one product to another.">>,
            key_insight = <<"High switching costs create customer lock-in">>,
            application = <<"Product strategy, customer retention, competitive moats">>,
            failure_modes = [<<"Customer resentment">>, <<"Disruption vulnerability">>],
            keywords = [<<"switching">>, <<"cost">>, <<"lock-in">>, <<"change">>, <<"migrate">>]
        },
        #model{
            id = <<"brand">>,
            name = <<"Brand">>,
            category = <<"Organizational">>,
            description = <<"The accumulated perception and trust associated with a name or symbol.">>,
            key_insight = <<"Brand is a promise that reduces customer uncertainty">>,
            application = <<"Marketing, pricing power, competitive positioning">>,
            failure_modes = [<<"Brand dilution">>, <<"Reputation damage">>],
            keywords = [<<"brand">>, <<"reputation">>, <<"trust">>, <<"perception">>, <<"name">>]
        },
        #model{
            id = <<"principal_agent">>,
            name = <<"Principal-Agent Problem">>,
            category = <<"Organizational">>,
            description = <<"Conflicts arise when one party (agent) acts on behalf of another (principal).">>,
            key_insight = <<"Agents optimize for their own interests, not principals'">>,
            application = <<"Governance, compensation design, outsourcing">>,
            failure_modes = [<<"Moral hazard">>, <<"Adverse selection">>],
            keywords = [<<"principal">>, <<"agent">>, <<"conflict">>, <<"interest">>, <<"incentive">>]
        },
        #model{
            id = <<"bureaucracy">>,
            name = <<"Bureaucracy">>,
            category = <<"Organizational">>,
            description = <<"Administrative systems that prioritize rules and procedures over outcomes.">>,
            key_insight = <<"Bureaucracy trades efficiency for consistency and control">>,
            application = <<"Organizational design, process improvement, scaling">>,
            failure_modes = [<<"Red tape">>, <<"Innovation stifling">>],
            keywords = [<<"bureaucracy">>, <<"rules">>, <<"procedure">>, <<"administration">>, <<"process">>]
        },

        %% ============================================
        %% PHYSICS & ENGINEERING
        %% ============================================
        #model{
            id = <<"critical_mass">>,
            name = <<"Critical Mass">>,
            category = <<"Physics & Engineering">>,
            description = <<"The minimum amount needed to sustain a chain reaction or achieve a tipping point.">>,
            key_insight = <<"Below threshold, nothing happens; above it, everything changes">>,
            application = <<"Product launches, social movements, network effects">>,
            failure_modes = [<<"Premature scaling">>, <<"Underestimating threshold">>],
            keywords = [<<"critical">>, <<"mass">>, <<"threshold">>, <<"tipping">>, <<"chain">>]
        },
        #model{
            id = <<"leverage">>,
            name = <<"Leverage">>,
            category = <<"Physics & Engineering">>,
            description = <<"Using a small force to move a large weight through mechanical advantage.">>,
            key_insight = <<"Give me a lever long enough and I can move the world">>,
            application = <<"Business strategy, personal productivity, financial engineering">>,
            failure_modes = [<<"Over-leverage">>, <<"Fragility">>],
            keywords = [<<"leverage">>, <<"amplify">>, <<"multiply">>, <<"force">>, <<"advantage">>]
        },
        #model{
            id = <<"activation_energy">>,
            name = <<"Activation Energy">>,
            category = <<"Physics & Engineering">>,
            description = <<"The minimum energy required to start a reaction or process.">>,
            key_insight = <<"Getting started is often the hardest part">>,
            application = <<"Habit formation, project initiation, behavior change">>,
            failure_modes = [<<"Procrastination">>, <<"Inertia">>],
            keywords = [<<"activation">>, <<"energy">>, <<"start">>, <<"initiate">>, <<"barrier">>]
        },
        #model{
            id = <<"entropy">>,
            name = <<"Entropy">>,
            category = <<"Physics & Engineering">>,
            description = <<"Systems naturally move toward disorder without energy input.">>,
            key_insight = <<"Order requires constant maintenance">>,
            application = <<"Organizational maintenance, relationship upkeep, system design">>,
            failure_modes = [<<"Neglect">>, <<"Decay">>],
            keywords = [<<"entropy">>, <<"disorder">>, <<"decay">>, <<"maintenance">>, <<"chaos">>]
        },
        #model{
            id = <<"redundancy">>,
            name = <<"Redundancy">>,
            category = <<"Physics & Engineering">>,
            description = <<"Building backup systems to prevent single points of failure.">>,
            key_insight = <<"Critical systems need multiple independent backups">>,
            application = <<"System design, risk management, safety engineering">>,
            failure_modes = [<<"Cost overhead">>, <<"Complexity">>],
            keywords = [<<"redundancy">>, <<"backup">>, <<"failsafe">>, <<"duplicate">>, <<"reserve">>]
        },
        #model{
            id = <<"breakpoints">>,
            name = <<"Breakpoints">>,
            category = <<"Physics & Engineering">>,
            description = <<"Points where systems change behavior dramatically or fail.">>,
            key_insight = <<"Systems often fail suddenly, not gradually">>,
            application = <<"Stress testing, capacity planning, risk assessment">>,
            failure_modes = [<<"Ignoring limits">>, <<"Overconfidence">>],
            keywords = [<<"breakpoint">>, <<"failure">>, <<"limit">>, <<"threshold">>, <<"stress">>]
        },
        #model{
            id = <<"half_life">>,
            name = <<"Half-Life">>,
            category = <<"Physics & Engineering">>,
            description = <<"The time for something to decay to half its value.">>,
            key_insight = <<"Knowledge and skills decay at predictable rates">>,
            application = <<"Learning strategy, content planning, investment timing">>,
            failure_modes = [<<"Outdated knowledge">>, <<"Skill atrophy">>],
            keywords = [<<"half-life">>, <<"decay">>, <<"depreciation">>, <<"obsolescence">>]
        },

        %% ============================================
        %% BIOLOGY & EVOLUTION
        %% ============================================
        #model{
            id = <<"natural_selection">>,
            name = <<"Natural Selection">>,
            category = <<"Biology & Evolution">>,
            description = <<"Organisms better adapted to their environment tend to survive and reproduce.">>,
            key_insight = <<"Adaptation to environment determines survival">>,
            application = <<"Market competition, product evolution, organizational fitness">>,
            failure_modes = [<<"Maladaptation">>, <<"Environmental mismatch">>],
            keywords = [<<"selection">>, <<"adaptation">>, <<"survival">>, <<"fitness">>, <<"evolution">>]
        },
        #model{
            id = <<"red_queen_effect">>,
            name = <<"Red Queen Effect">>,
            category = <<"Biology & Evolution">>,
            description = <<"You must run faster just to stay in place as competitors evolve.">>,
            key_insight = <<"Standing still means falling behind">>,
            application = <<"Competitive strategy, continuous improvement, arms races">>,
            failure_modes = [<<"Exhaustion">>, <<"Resource depletion">>],
            keywords = [<<"red queen">>, <<"competition">>, <<"evolution">>, <<"race">>, <<"adapt">>]
        },
        #model{
            id = <<"adaptation">>,
            name = <<"Adaptation">>,
            category = <<"Biology & Evolution">>,
            description = <<"Organisms change to better fit their environment over time.">>,
            key_insight = <<"What works in one environment may fail in another">>,
            application = <<"Change management, market entry, personal development">>,
            failure_modes = [<<"Over-specialization">>, <<"Rigidity">>],
            keywords = [<<"adapt">>, <<"change">>, <<"environment">>, <<"fit">>, <<"evolve">>]
        },
        #model{
            id = <<"symbiosis">>,
            name = <<"Symbiosis">>,
            category = <<"Biology & Evolution">>,
            description = <<"Close relationships between different species that benefit one or both.">>,
            key_insight = <<"Cooperation can be more powerful than competition">>,
            application = <<"Partnerships, ecosystems, platform businesses">>,
            failure_modes = [<<"Parasitism">>, <<"Dependency">>],
            keywords = [<<"symbiosis">>, <<"mutualism">>, <<"cooperation">>, <<"partnership">>]
        },
        #model{
            id = <<"niche">>,
            name = <<"Ecological Niche">>,
            category = <<"Biology & Evolution">>,
            description = <<"The specific role and position an organism occupies in its ecosystem.">>,
            key_insight = <<"Specialization reduces competition">>,
            application = <<"Market positioning, career strategy, product differentiation">>,
            failure_modes = [<<"Niche collapse">>, <<"Over-specialization">>],
            keywords = [<<"niche">>, <<"specialization">>, <<"position">>, <<"role">>, <<"ecosystem">>]
        },

        %% ============================================
        %% MATHEMATICS & STATISTICS
        %% ============================================
        #model{
            id = <<"power_law">>,
            name = <<"Power Law">>,
            category = <<"Mathematics">>,
            description = <<"A few items account for most of the effect (80/20 rule).">>,
            key_insight = <<"Distributions are often extremely unequal">>,
            application = <<"Resource allocation, prioritization, venture capital">>,
            failure_modes = [<<"Ignoring the tail">>, <<"Over-concentration">>],
            keywords = [<<"power law">>, <<"pareto">>, <<"80/20">>, <<"distribution">>, <<"inequality">>]
        },
        #model{
            id = <<"regression_to_mean">>,
            name = <<"Regression to the Mean">>,
            category = <<"Mathematics">>,
            description = <<"Extreme results tend to be followed by more average ones.">>,
            key_insight = <<"Exceptional performance often reverts to normal">>,
            application = <<"Performance evaluation, sports analysis, investment returns">>,
            failure_modes = [<<"Overreacting to outliers">>, <<"False patterns">>],
            keywords = [<<"regression">>, <<"mean">>, <<"average">>, <<"reversion">>, <<"normal">>]
        },
        #model{
            id = <<"bayes_theorem">>,
            name = <<"Bayes' Theorem">>,
            category = <<"Mathematics">>,
            description = <<"Update probability estimates based on new evidence.">>,
            key_insight = <<"Prior beliefs should be updated rationally with new information">>,
            application = <<"Medical diagnosis, spam filtering, decision-making">>,
            failure_modes = [<<"Ignoring base rates">>, <<"Overweighting new evidence">>],
            keywords = [<<"bayes">>, <<"probability">>, <<"update">>, <<"prior">>, <<"evidence">>]
        },
        #model{
            id = <<"law_of_large_numbers">>,
            name = <<"Law of Large Numbers">>,
            category = <<"Mathematics">>,
            description = <<"As sample size increases, results approach the expected value.">>,
            key_insight = <<"Small samples are unreliable; large samples reveal truth">>,
            application = <<"Insurance, gambling, A/B testing">>,
            failure_modes = [<<"Small sample fallacy">>, <<"Gambler's fallacy">>],
            keywords = [<<"large numbers">>, <<"sample">>, <<"average">>, <<"expected">>, <<"converge">>]
        },
        #model{
            id = <<"compounding">>,
            name = <<"Compounding">>,
            category = <<"Mathematics">>,
            description = <<"Growth on growth creates exponential results over time.">>,
            key_insight = <<"Small consistent gains become enormous over time">>,
            application = <<"Investing, learning, relationship building">>,
            failure_modes = [<<"Impatience">>, <<"Interrupting the process">>],
            keywords = [<<"compound">>, <<"exponential">>, <<"growth">>, <<"interest">>, <<"accumulate">>]
        },
        #model{
            id = <<"normal_distribution">>,
            name = <<"Normal Distribution">>,
            category = <<"Mathematics">>,
            description = <<"Many phenomena cluster around an average with predictable spread.">>,
            key_insight = <<"Most outcomes are average; extremes are rare">>,
            application = <<"Quality control, grading, risk assessment">>,
            failure_modes = [<<"Assuming normality">>, <<"Fat tail blindness">>],
            keywords = [<<"normal">>, <<"bell curve">>, <<"gaussian">>, <<"average">>, <<"standard deviation">>]
        },
        #model{
            id = <<"fat_tails">>,
            name = <<"Fat Tails">>,
            category = <<"Mathematics">>,
            description = <<"Extreme events occur more often than normal distribution predicts.">>,
            key_insight = <<"Black swans are more common than we think">>,
            application = <<"Risk management, financial modeling, disaster planning">>,
            failure_modes = [<<"Underestimating extremes">>, <<"Model failure">>],
            keywords = [<<"fat tails">>, <<"black swan">>, <<"extreme">>, <<"outlier">>, <<"risk">>]
        },

        %% ============================================
        %% ECONOMICS & MARKETS
        %% ============================================
        #model{
            id = <<"supply_demand">>,
            name = <<"Supply and Demand">>,
            category = <<"Economics">>,
            description = <<"Prices adjust to balance what sellers offer and buyers want.">>,
            key_insight = <<"Scarcity and desire determine value">>,
            application = <<"Pricing, market analysis, resource allocation">>,
            failure_modes = [<<"Price controls">>, <<"Market manipulation">>],
            keywords = [<<"supply">>, <<"demand">>, <<"price">>, <<"market">>, <<"equilibrium">>]
        },
        #model{
            id = <<"opportunity_cost">>,
            name = <<"Opportunity Cost">>,
            category = <<"Economics">>,
            description = <<"The value of the next best alternative foregone.">>,
            key_insight = <<"Every choice has a hidden cost">>,
            application = <<"Decision-making, resource allocation, time management">>,
            failure_modes = [<<"Ignoring alternatives">>, <<"Sunk cost fallacy">>],
            keywords = [<<"opportunity">>, <<"cost">>, <<"alternative">>, <<"trade-off">>, <<"foregone">>]
        },
        #model{
            id = <<"comparative_advantage">>,
            name = <<"Comparative Advantage">>,
            category = <<"Economics">>,
            description = <<"Focus on what you do relatively better, even if not absolutely best.">>,
            key_insight = <<"Trade benefits everyone when each specializes">>,
            application = <<"Career choices, outsourcing, international trade">>,
            failure_modes = [<<"Ignoring absolute advantage">>, <<"Over-specialization">>],
            keywords = [<<"comparative">>, <<"advantage">>, <<"specialization">>, <<"trade">>, <<"relative">>]
        },
        #model{
            id = <<"creative_destruction">>,
            name = <<"Creative Destruction">>,
            category = <<"Economics">>,
            description = <<"Innovation destroys old industries while creating new ones.">>,
            key_insight = <<"Progress requires letting go of the past">>,
            application = <<"Industry analysis, career planning, investment">>,
            failure_modes = [<<"Resistance to change">>, <<"Disruption blindness">>],
            keywords = [<<"creative">>, <<"destruction">>, <<"innovation">>, <<"disruption">>, <<"progress">>]
        },
        #model{
            id = <<"tragedy_of_commons">>,
            name = <<"Tragedy of the Commons">>,
            category = <<"Economics">>,
            description = <<"Shared resources get depleted when individuals act in self-interest.">>,
            key_insight = <<"Individual rationality can lead to collective ruin">>,
            application = <<"Environmental policy, resource management, team dynamics">>,
            failure_modes = [<<"Free riding">>, <<"Overexploitation">>],
            keywords = [<<"commons">>, <<"shared">>, <<"depletion">>, <<"collective">>, <<"tragedy">>]
        },
        #model{
            id = <<"asymmetric_information">>,
            name = <<"Asymmetric Information">>,
            category = <<"Economics">>,
            description = <<"One party has more or better information than another.">>,
            key_insight = <<"Information imbalance creates market inefficiencies">>,
            application = <<"Negotiation, hiring, insurance">>,
            failure_modes = [<<"Adverse selection">>, <<"Moral hazard">>],
            keywords = [<<"asymmetric">>, <<"information">>, <<"knowledge">>, <<"imbalance">>, <<"hidden">>]
        },
        #model{
            id = <<"game_theory">>,
            name = <<"Game Theory">>,
            category = <<"Economics">>,
            description = <<"Strategic decision-making where outcomes depend on others' choices.">>,
            key_insight = <<"Anticipate how others will respond to your moves">>,
            application = <<"Negotiation, competitive strategy, auction design">>,
            failure_modes = [<<"Assuming rationality">>, <<"Ignoring emotions">>],
            keywords = [<<"game theory">>, <<"strategy">>, <<"nash">>, <<"equilibrium">>, <<"payoff">>]
        },
        #model{
            id = <<"prisoners_dilemma">>,
            name = <<"Prisoner's Dilemma">>,
            category = <<"Economics">>,
            description = <<"Individual incentives lead to worse outcomes for all.">>,
            key_insight = <<"Cooperation requires trust or enforcement">>,
            application = <<"Negotiations, arms races, business competition">>,
            failure_modes = [<<"Defection spirals">>, <<"Trust breakdown">>],
            keywords = [<<"prisoner">>, <<"dilemma">>, <<"cooperation">>, <<"defection">>, <<"trust">>]
        },

        %% ============================================
        %% SYSTEMS THINKING
        %% ============================================
        #model{
            id = <<"feedback_loops">>,
            name = <<"Feedback Loops">>,
            category = <<"Systems Thinking">>,
            description = <<"Outputs of a system feed back as inputs, amplifying or dampening effects.">>,
            key_insight = <<"Small changes can create large effects through feedback">>,
            application = <<"System design, habit formation, organizational change">>,
            failure_modes = [<<"Runaway loops">>, <<"Oscillation">>],
            keywords = [<<"feedback">>, <<"loop">>, <<"amplify">>, <<"dampen">>, <<"cycle">>]
        },
        #model{
            id = <<"emergence">>,
            name = <<"Emergence">>,
            category = <<"Systems Thinking">>,
            description = <<"Complex patterns arise from simple rules and interactions.">>,
            key_insight = <<"The whole is greater than the sum of its parts">>,
            application = <<"Organizational design, AI, urban planning">>,
            failure_modes = [<<"Unpredictability">>, <<"Unintended consequences">>],
            keywords = [<<"emergence">>, <<"complex">>, <<"simple">>, <<"pattern">>, <<"self-organize">>]
        },
        #model{
            id = <<"bottleneck">>,
            name = <<"Bottleneck">>,
            category = <<"Systems Thinking">>,
            description = <<"The constraint that limits the throughput of an entire system.">>,
            key_insight = <<"Improving non-bottlenecks doesn't improve the system">>,
            application = <<"Process optimization, capacity planning, project management">>,
            failure_modes = [<<"Wrong focus">>, <<"Moving bottlenecks">>],
            keywords = [<<"bottleneck">>, <<"constraint">>, <<"throughput">>, <<"limit">>, <<"capacity">>]
        },
        #model{
            id = <<"homeostasis">>,
            name = <<"Homeostasis">>,
            category = <<"Systems Thinking">>,
            description = <<"Systems tend to maintain stable internal conditions.">>,
            key_insight = <<"Change is resisted by stabilizing forces">>,
            application = <<"Change management, habit breaking, organizational reform">>,
            failure_modes = [<<"Resistance to change">>, <<"Stagnation">>],
            keywords = [<<"homeostasis">>, <<"stability">>, <<"equilibrium">>, <<"balance">>, <<"resist">>]
        },
        #model{
            id = <<"path_dependence">>,
            name = <<"Path Dependence">>,
            category = <<"Systems Thinking">>,
            description = <<"Current options are limited by past decisions.">>,
            key_insight = <<"History constrains the future">>,
            application = <<"Technology adoption, career planning, institutional design">>,
            failure_modes = [<<"Lock-in">>, <<"Suboptimal equilibria">>],
            keywords = [<<"path">>, <<"dependence">>, <<"history">>, <<"lock-in">>, <<"legacy">>]
        },
        #model{
            id = <<"resilience">>,
            name = <<"Resilience">>,
            category = <<"Systems Thinking">>,
            description = <<"The ability to absorb shocks and recover from disruption.">>,
            key_insight = <<"Resilient systems bend but don't break">>,
            application = <<"Risk management, organizational design, personal development">>,
            failure_modes = [<<"Brittleness">>, <<"Over-optimization">>],
            keywords = [<<"resilience">>, <<"robust">>, <<"recover">>, <<"shock">>, <<"adapt">>]
        },
        #model{
            id = <<"antifragility">>,
            name = <<"Antifragility">>,
            category = <<"Systems Thinking">>,
            description = <<"Systems that gain from disorder and stress.">>,
            key_insight = <<"Some things benefit from volatility">>,
            application = <<"Portfolio design, career strategy, system architecture">>,
            failure_modes = [<<"Excessive stress">>, <<"Breaking point">>],
            keywords = [<<"antifragile">>, <<"stress">>, <<"volatility">>, <<"gain">>, <<"disorder">>]
        },

        %% ============================================
        %% MILITARY & STRATEGY
        %% ============================================
        #model{
            id = <<"asymmetric_warfare">>,
            name = <<"Asymmetric Warfare">>,
            category = <<"Military & Strategy">>,
            description = <<"Weaker parties use unconventional tactics against stronger opponents.">>,
            key_insight = <<"Don't fight on your opponent's terms">>,
            application = <<"Competitive strategy, startup tactics, negotiation">>,
            failure_modes = [<<"Escalation">>, <<"Retaliation">>],
            keywords = [<<"asymmetric">>, <<"guerrilla">>, <<"unconventional">>, <<"underdog">>]
        },
        #model{
            id = <<"flanking">>,
            name = <<"Flanking">>,
            category = <<"Military & Strategy">>,
            description = <<"Attack from the side where defenses are weakest.">>,
            key_insight = <<"Avoid frontal assaults on strength">>,
            application = <<"Market entry, competitive positioning, negotiation">>,
            failure_modes = [<<"Overextension">>, <<"Counter-flanking">>],
            keywords = [<<"flank">>, <<"side">>, <<"weakness">>, <<"indirect">>, <<"maneuver">>]
        },
        #model{
            id = <<"fog_of_war">>,
            name = <<"Fog of War">>,
            category = <<"Military & Strategy">>,
            description = <<"Uncertainty and incomplete information in complex situations.">>,
            key_insight = <<"Plans rarely survive first contact with reality">>,
            application = <<"Project planning, crisis management, decision-making">>,
            failure_modes = [<<"Paralysis">>, <<"Overconfidence">>],
            keywords = [<<"fog">>, <<"war">>, <<"uncertainty">>, <<"incomplete">>, <<"chaos">>]
        },
        #model{
            id = <<"force_multiplier">>,
            name = <<"Force Multiplier">>,
            category = <<"Military & Strategy">>,
            description = <<"Factors that dramatically increase effectiveness.">>,
            key_insight = <<"Some advantages compound your other advantages">>,
            application = <<"Team building, technology adoption, skill development">>,
            failure_modes = [<<"Over-reliance">>, <<"Single point of failure">>],
            keywords = [<<"force">>, <<"multiplier">>, <<"amplify">>, <<"effectiveness">>, <<"leverage">>]
        },
        #model{
            id = <<"schwerpunkt">>,
            name = <<"Schwerpunkt (Focus Point)">>,
            category = <<"Military & Strategy">>,
            description = <<"Concentrate overwhelming force at the decisive point.">>,
            key_insight = <<"Dispersion leads to defeat; concentration wins">>,
            application = <<"Resource allocation, product strategy, campaign planning">>,
            failure_modes = [<<"Wrong focus">>, <<"Neglecting other areas">>],
            keywords = [<<"focus">>, <<"concentrate">>, <<"decisive">>, <<"overwhelming">>, <<"priority">>]
        },

        %% ============================================
        %% PSYCHOLOGY: ADVANCED
        %% ============================================
        #model{
            id = <<"social_proof">>,
            name = <<"Social Proof">>,
            category = <<"Psychology: Advanced">>,
            description = <<"People follow what others are doing, especially in uncertainty.">>,
            key_insight = <<"We look to others to determine correct behavior">>,
            application = <<"Marketing, leadership, behavior change">>,
            failure_modes = [<<"Herd behavior">>, <<"Manipulation">>],
            keywords = [<<"social">>, <<"proof">>, <<"conformity">>, <<"herd">>, <<"follow">>]
        },
        #model{
            id = <<"authority_bias">>,
            name = <<"Authority Bias">>,
            category = <<"Psychology: Advanced">>,
            description = <<"Tendency to attribute greater accuracy to authority figures.">>,
            key_insight = <<"Credentials don't guarantee correctness">>,
            application = <<"Critical thinking, hiring, expert evaluation">>,
            failure_modes = [<<"Blind obedience">>, <<"Expert worship">>],
            keywords = [<<"authority">>, <<"expert">>, <<"obedience">>, <<"credentials">>, <<"trust">>]
        },
        #model{
            id = <<"liking_bias">>,
            name = <<"Liking Bias">>,
            category = <<"Psychology: Advanced">>,
            description = <<"We are more easily influenced by people we like.">>,
            key_insight = <<"Likability is a powerful persuasion tool">>,
            application = <<"Sales, negotiation, relationship building">>,
            failure_modes = [<<"Manipulation">>, <<"Poor judgment">>],
            keywords = [<<"liking">>, <<"likability">>, <<"influence">>, <<"persuasion">>, <<"rapport">>]
        },
        #model{
            id = <<"commitment_consistency">>,
            name = <<"Commitment and Consistency">>,
            category = <<"Psychology: Advanced">>,
            description = <<"Once committed, people strive to be consistent with that commitment.">>,
            key_insight = <<"Small commitments lead to larger ones">>,
            application = <<"Sales, behavior change, goal setting">>,
            failure_modes = [<<"Foolish consistency">>, <<"Escalation of commitment">>],
            keywords = [<<"commitment">>, <<"consistency">>, <<"foot-in-door">>, <<"promise">>]
        },
        #model{
            id = <<"scarcity">>,
            name = <<"Scarcity">>,
            category = <<"Psychology: Advanced">>,
            description = <<"Things seem more valuable when they are rare or diminishing.">>,
            key_insight = <<"Fear of missing out drives behavior">>,
            application = <<"Marketing, negotiation, pricing">>,
            failure_modes = [<<"Artificial scarcity">>, <<"Panic buying">>],
            keywords = [<<"scarcity">>, <<"rare">>, <<"limited">>, <<"fomo">>, <<"exclusive">>]
        },
        #model{
            id = <<"reciprocity">>,
            name = <<"Reciprocity">>,
            category = <<"Psychology: Advanced">>,
            description = <<"People feel obligated to return favors.">>,
            key_insight = <<"Give first to receive later">>,
            application = <<"Networking, sales, relationship building">>,
            failure_modes = [<<"Exploitation">>, <<"Unwanted obligations">>],
            keywords = [<<"reciprocity">>, <<"favor">>, <<"give">>, <<"return">>, <<"obligation">>]
        },
        #model{
            id = <<"dunning_kruger">>,
            name = <<"Dunning-Kruger Effect">>,
            category = <<"Psychology: Advanced">>,
            description = <<"Unskilled people overestimate ability; skilled people underestimate.">>,
            key_insight = <<"Confidence and competence are often inversely related">>,
            application = <<"Hiring, self-assessment, team building">>,
            failure_modes = [<<"Overconfidence">>, <<"Imposter syndrome">>],
            keywords = [<<"dunning">>, <<"kruger">>, <<"overconfidence">>, <<"competence">>, <<"skill">>]
        },
        #model{
            id = <<"narrative_fallacy">>,
            name = <<"Narrative Fallacy">>,
            category = <<"Psychology: Advanced">>,
            description = <<"We create stories to explain random events.">>,
            key_insight = <<"Humans need stories, even when none exist">>,
            application = <<"Analysis, forecasting, history interpretation">>,
            failure_modes = [<<"False causation">>, <<"Oversimplification">>],
            keywords = [<<"narrative">>, <<"story">>, <<"causation">>, <<"pattern">>, <<"meaning">>]
        },
        #model{
            id = <<"peak_end_rule">>,
            name = <<"Peak-End Rule">>,
            category = <<"Psychology: Advanced">>,
            description = <<"Experiences are judged by their peak and end, not average.">>,
            key_insight = <<"Endings matter more than duration">>,
            application = <<"Customer experience, event planning, presentations">>,
            failure_modes = [<<"Neglecting the middle">>, <<"Bad endings">>],
            keywords = [<<"peak">>, <<"end">>, <<"memory">>, <<"experience">>, <<"judgment">>]
        },

        %% ============================================
        %% DECISION-MAKING
        %% ============================================
        #model{
            id = <<"reversibility">>,
            name = <<"Reversibility">>,
            category = <<"Decision-Making">>,
            description = <<"Distinguish between reversible and irreversible decisions.">>,
            key_insight = <<"Reversible decisions should be made quickly">>,
            application = <<"Decision speed, risk assessment, resource allocation">>,
            failure_modes = [<<"Over-deliberation">>, <<"Recklessness">>],
            keywords = [<<"reversible">>, <<"irreversible">>, <<"decision">>, <<"speed">>, <<"undo">>]
        },
        #model{
            id = <<"satisficing">>,
            name = <<"Satisficing">>,
            category = <<"Decision-Making">>,
            description = <<"Choose the first option that meets minimum criteria.">>,
            key_insight = <<"Good enough is often better than optimal">>,
            application = <<"Time management, hiring, purchasing">>,
            failure_modes = [<<"Settling too low">>, <<"Missing better options">>],
            keywords = [<<"satisfice">>, <<"good enough">>, <<"threshold">>, <<"minimum">>, <<"acceptable">>]
        },
        #model{
            id = <<"maximizing">>,
            name = <<"Maximizing">>,
            category = <<"Decision-Making">>,
            description = <<"Exhaustively search for the best possible option.">>,
            key_insight = <<"Perfectionism has diminishing returns">>,
            application = <<"Major decisions, high-stakes choices">>,
            failure_modes = [<<"Analysis paralysis">>, <<"Regret">>],
            keywords = [<<"maximize">>, <<"optimal">>, <<"best">>, <<"perfect">>, <<"exhaustive">>]
        },
        #model{
            id = <<"optionality">>,
            name = <<"Optionality">>,
            category = <<"Decision-Making">>,
            description = <<"Preserve choices and flexibility for the future.">>,
            key_insight = <<"Options have value even if never exercised">>,
            application = <<"Career planning, investing, strategic planning">>,
            failure_modes = [<<"Indecision">>, <<"Option hoarding">>],
            keywords = [<<"option">>, <<"flexibility">>, <<"choice">>, <<"preserve">>, <<"future">>]
        },
        #model{
            id = <<"margin_of_safety">>,
            name = <<"Margin of Safety">>,
            category = <<"Decision-Making">>,
            description = <<"Build in buffers for errors and unexpected events.">>,
            key_insight = <<"Plan for things to go wrong">>,
            application = <<"Engineering, investing, project planning">>,
            failure_modes = [<<"Over-engineering">>, <<"Wasted resources">>],
            keywords = [<<"margin">>, <<"safety">>, <<"buffer">>, <<"cushion">>, <<"conservative">>]
        },
        #model{
            id = <<"via_negativa">>,
            name = <<"Via Negativa">>,
            category = <<"Decision-Making">>,
            description = <<"Improve by removing rather than adding.">>,
            key_insight = <<"Subtraction is often more powerful than addition">>,
            application = <<"Process improvement, health, simplification">>,
            failure_modes = [<<"Over-removal">>, <<"Losing essentials">>],
            keywords = [<<"via negativa">>, <<"subtract">>, <<"remove">>, <<"eliminate">>, <<"simplify">>]
        },

        %% ============================================
        %% COMMUNICATION & INFLUENCE
        %% ============================================
        #model{
            id = <<"framing">>,
            name = <<"Framing">>,
            category = <<"Communication">>,
            description = <<"How information is presented affects how it's perceived.">>,
            key_insight = <<"The frame changes the picture">>,
            application = <<"Negotiation, marketing, leadership">>,
            failure_modes = [<<"Manipulation">>, <<"Misrepresentation">>],
            keywords = [<<"frame">>, <<"present">>, <<"context">>, <<"perception">>, <<"spin">>]
        },
        #model{
            id = <<"storytelling">>,
            name = <<"Storytelling">>,
            category = <<"Communication">>,
            description = <<"Narratives are more memorable and persuasive than facts alone.">>,
            key_insight = <<"Stories bypass rational resistance">>,
            application = <<"Leadership, sales, teaching">>,
            failure_modes = [<<"Manipulation">>, <<"Oversimplification">>],
            keywords = [<<"story">>, <<"narrative">>, <<"persuade">>, <<"memorable">>, <<"engage">>]
        },
        #model{
            id = <<"steel_manning">>,
            name = <<"Steel Manning">>,
            category = <<"Communication">>,
            description = <<"Present the strongest version of opposing arguments.">>,
            key_insight = <<"Understanding opponents strengthens your position">>,
            application = <<"Debate, decision-making, conflict resolution">>,
            failure_modes = [<<"Wasting time">>, <<"Appearing weak">>],
            keywords = [<<"steel man">>, <<"argument">>, <<"opponent">>, <<"strongest">>, <<"fair">>]
        },
        #model{
            id = <<"socratic_method">>,
            name = <<"Socratic Method">>,
            category = <<"Communication">>,
            description = <<"Use questions to stimulate critical thinking and illuminate ideas.">>,
            key_insight = <<"Questions are more powerful than statements">>,
            application = <<"Teaching, coaching, discovery">>,
            failure_modes = [<<"Frustration">>, <<"Manipulation">>],
            keywords = [<<"socratic">>, <<"question">>, <<"inquiry">>, <<"think">>, <<"discover">>]
        },

        %% ============================================
        %% INVESTING & FINANCE
        %% ============================================
        #model{
            id = <<"mr_market">>,
            name = <<"Mr. Market">>,
            category = <<"Investing">>,
            description = <<"The market is an emotional partner offering prices, not a guide to value.">>,
            key_insight = <<"Market prices reflect sentiment, not always value">>,
            application = <<"Value investing, emotional discipline, contrarian thinking">>,
            failure_modes = [<<"Ignoring market signals">>, <<"Stubbornness">>],
            keywords = [<<"market">>, <<"emotion">>, <<"value">>, <<"price">>, <<"sentiment">>]
        },
        #model{
            id = <<"moat">>,
            name = <<"Economic Moat">>,
            category = <<"Investing">>,
            description = <<"Sustainable competitive advantages that protect profits.">>,
            key_insight = <<"Durable advantages compound over time">>,
            application = <<"Business analysis, competitive strategy, investment selection">>,
            failure_modes = [<<"Moat erosion">>, <<"Disruption">>],
            keywords = [<<"moat">>, <<"competitive">>, <<"advantage">>, <<"durable">>, <<"protect">>]
        },
        #model{
            id = <<"intrinsic_value">>,
            name = <<"Intrinsic Value">>,
            category = <<"Investing">>,
            description = <<"The true worth of an asset based on fundamentals, not market price.">>,
            key_insight = <<"Price is what you pay; value is what you get">>,
            application = <<"Investment analysis, negotiation, business valuation">>,
            failure_modes = [<<"Miscalculation">>, <<"Changing fundamentals">>],
            keywords = [<<"intrinsic">>, <<"value">>, <<"fundamental">>, <<"worth">>, <<"price">>]
        },
        #model{
            id = <<"circle_of_competence_investing">>,
            name = <<"Circle of Competence (Investing)">>,
            category = <<"Investing">>,
            description = <<"Only invest in businesses you truly understand.">>,
            key_insight = <<"Knowing what you don't know prevents costly mistakes">>,
            application = <<"Portfolio construction, due diligence, risk management">>,
            failure_modes = [<<"Overconfidence">>, <<"Missed opportunities">>],
            keywords = [<<"competence">>, <<"understand">>, <<"knowledge">>, <<"expertise">>]
        },
        #model{
            id = <<"skin_in_game">>,
            name = <<"Skin in the Game">>,
            category = <<"Investing">>,
            description = <<"Having personal risk aligned with outcomes improves decision quality.">>,
            key_insight = <<"People behave differently when they bear consequences">>,
            application = <<"Governance, incentive design, trust assessment">>,
            failure_modes = [<<"Risk aversion">>, <<"Short-termism">>],
            keywords = [<<"skin">>, <<"game">>, <<"risk">>, <<"aligned">>, <<"consequences">>]
        },

        %% ============================================
        %% TECHNOLOGY & INNOVATION
        %% ============================================
        #model{
            id = <<"moores_law">>,
            name = <<"Moore's Law">>,
            category = <<"Technology">>,
            description = <<"Computing power doubles approximately every two years.">>,
            key_insight = <<"Exponential improvement makes impossible things possible">>,
            application = <<"Technology planning, product roadmaps, timing decisions">>,
            failure_modes = [<<"Physical limits">>, <<"Extrapolation errors">>],
            keywords = [<<"moore">>, <<"exponential">>, <<"computing">>, <<"double">>, <<"growth">>]
        },
        #model{
            id = <<"network_effects">>,
            name = <<"Network Effects">>,
            category = <<"Technology">>,
            description = <<"A product becomes more valuable as more people use it.">>,
            key_insight = <<"Winner-take-all dynamics emerge from network effects">>,
            application = <<"Platform strategy, market timing, competitive analysis">>,
            failure_modes = [<<"Chicken-and-egg">>, <<"Network collapse">>],
            keywords = [<<"network">>, <<"effect">>, <<"platform">>, <<"users">>, <<"value">>]
        },
        #model{
            id = <<"s_curve">>,
            name = <<"S-Curve">>,
            category = <<"Technology">>,
            description = <<"Technologies follow slow start, rapid growth, then plateau.">>,
            key_insight = <<"Timing matters - early and late adopters face different risks">>,
            application = <<"Technology adoption, market timing, innovation strategy">>,
            failure_modes = [<<"Premature scaling">>, <<"Missing the curve">>],
            keywords = [<<"s-curve">>, <<"adoption">>, <<"growth">>, <<"plateau">>, <<"timing">>]
        },
        #model{
            id = <<"disruptive_innovation">>,
            name = <<"Disruptive Innovation">>,
            category = <<"Technology">>,
            description = <<"Inferior products can displace incumbents by serving overlooked segments.">>,
            key_insight = <<"Disruption starts at the bottom, not the top">>,
            application = <<"Competitive strategy, market entry, threat assessment">>,
            failure_modes = [<<"Sustaining vs disruptive confusion">>, <<"Overreaction">>],
            keywords = [<<"disruptive">>, <<"innovation">>, <<"incumbent">>, <<"low-end">>, <<"christensen">>]
        },
        #model{
            id = <<"platform_business">>,
            name = <<"Platform Business Model">>,
            category = <<"Technology">>,
            description = <<"Create value by facilitating exchanges between two or more groups.">>,
            key_insight = <<"Platforms scale better than pipelines">>,
            application = <<"Business model design, market strategy, ecosystem building">>,
            failure_modes = [<<"Multi-homing">>, <<"Disintermediation">>],
            keywords = [<<"platform">>, <<"marketplace">>, <<"two-sided">>, <<"ecosystem">>, <<"exchange">>]
        },

        %% ============================================
        %% LEADERSHIP & MANAGEMENT
        %% ============================================
        #model{
            id = <<"servant_leadership">>,
            name = <<"Servant Leadership">>,
            category = <<"Leadership">>,
            description = <<"Leaders exist to serve their teams, not the reverse.">>,
            key_insight = <<"The best leaders put others first">>,
            application = <<"Team management, organizational culture, coaching">>,
            failure_modes = [<<"Doormat syndrome">>, <<"Lack of direction">>],
            keywords = [<<"servant">>, <<"leadership">>, <<"serve">>, <<"team">>, <<"support">>]
        },
        #model{
            id = <<"delegation">>,
            name = <<"Delegation">>,
            category = <<"Leadership">>,
            description = <<"Assigning responsibility and authority to others.">>,
            key_insight = <<"You can delegate authority but not responsibility">>,
            application = <<"Management, scaling, team development">>,
            failure_modes = [<<"Micromanagement">>, <<"Abdication">>],
            keywords = [<<"delegate">>, <<"authority">>, <<"responsibility">>, <<"empower">>, <<"trust">>]
        },
        #model{
            id = <<"radical_candor">>,
            name = <<"Radical Candor">>,
            category = <<"Leadership">>,
            description = <<"Care personally while challenging directly.">>,
            key_insight = <<"Kindness without honesty is manipulation">>,
            application = <<"Feedback, coaching, team development">>,
            failure_modes = [<<"Obnoxious aggression">>, <<"Ruinous empathy">>],
            keywords = [<<"candor">>, <<"feedback">>, <<"honest">>, <<"care">>, <<"direct">>]
        },
        #model{
            id = <<"high_output_management">>,
            name = <<"High Output Management">>,
            category = <<"Leadership">>,
            description = <<"A manager's output is the output of their team.">>,
            key_insight = <<"Leverage your time through others">>,
            application = <<"Management, productivity, team building">>,
            failure_modes = [<<"Individual contributor mindset">>, <<"Neglecting team">>],
            keywords = [<<"output">>, <<"management">>, <<"team">>, <<"leverage">>, <<"grove">>]
        },
        #model{
            id = <<"mission_command">>,
            name = <<"Mission Command">>,
            category = <<"Leadership">>,
            description = <<"Give clear intent and let people figure out how to achieve it.">>,
            key_insight = <<"Decentralized execution with centralized intent">>,
            application = <<"Military, startups, distributed teams">>,
            failure_modes = [<<"Unclear intent">>, <<"Lack of trust">>],
            keywords = [<<"mission">>, <<"command">>, <<"intent">>, <<"autonomy">>, <<"decentralized">>]
        },

        %% ============================================
        %% LEARNING & KNOWLEDGE
        %% ============================================
        #model{
            id = <<"deliberate_practice">>,
            name = <<"Deliberate Practice">>,
            category = <<"Learning">>,
            description = <<"Focused, effortful practice at the edge of ability.">>,
            key_insight = <<"10,000 hours of wrong practice doesn't help">>,
            application = <<"Skill development, coaching, performance improvement">>,
            failure_modes = [<<"Mindless repetition">>, <<"Burnout">>],
            keywords = [<<"deliberate">>, <<"practice">>, <<"skill">>, <<"focus">>, <<"improve">>]
        },
        #model{
            id = <<"spaced_repetition">>,
            name = <<"Spaced Repetition">>,
            category = <<"Learning">>,
            description = <<"Review information at increasing intervals for better retention.">>,
            key_insight = <<"Forgetting is the enemy; spacing is the cure">>,
            application = <<"Studying, training, knowledge management">>,
            failure_modes = [<<"Over-scheduling">>, <<"Wrong material">>],
            keywords = [<<"spaced">>, <<"repetition">>, <<"memory">>, <<"retention">>, <<"interval">>]
        },
        #model{
            id = <<"feynman_technique">>,
            name = <<"Feynman Technique">>,
            category = <<"Learning">>,
            description = <<"Explain concepts simply to identify gaps in understanding.">>,
            key_insight = <<"If you can't explain it simply, you don't understand it">>,
            application = <<"Learning, teaching, communication">>,
            failure_modes = [<<"Oversimplification">>, <<"Missing nuance">>],
            keywords = [<<"feynman">>, <<"explain">>, <<"simple">>, <<"understand">>, <<"teach">>]
        },
        #model{
            id = <<"t_shaped_skills">>,
            name = <<"T-Shaped Skills">>,
            category = <<"Learning">>,
            description = <<"Deep expertise in one area with broad knowledge across many.">>,
            key_insight = <<"Specialists collaborate better with breadth">>,
            application = <<"Career development, team composition, hiring">>,
            failure_modes = [<<"Jack of all trades">>, <<"Narrow specialist">>],
            keywords = [<<"t-shaped">>, <<"specialist">>, <<"generalist">>, <<"breadth">>, <<"depth">>]
        },
        #model{
            id = <<"mental_models_latticework">>,
            name = <<"Mental Models Latticework">>,
            category = <<"Learning">>,
            description = <<"Build a network of interconnected mental models from multiple disciplines.">>,
            key_insight = <<"The person with the most models wins">>,
            application = <<"Decision-making, problem-solving, learning strategy">>,
            failure_modes = [<<"Model worship">>, <<"Analysis paralysis">>],
            keywords = [<<"mental models">>, <<"latticework">>, <<"munger">>, <<"multidisciplinary">>]
        },

        %% ============================================
        %% TIME & PRODUCTIVITY
        %% ============================================
        #model{
            id = <<"parkinsons_law">>,
            name = <<"Parkinson's Law">>,
            category = <<"Productivity">>,
            description = <<"Work expands to fill the time available for completion.">>,
            key_insight = <<"Deadlines create focus">>,
            application = <<"Time management, project planning, productivity">>,
            failure_modes = [<<"Rushed work">>, <<"Burnout">>],
            keywords = [<<"parkinson">>, <<"time">>, <<"deadline">>, <<"expand">>, <<"fill">>]
        },
        #model{
            id = <<"eisenhower_matrix">>,
            name = <<"Eisenhower Matrix">>,
            category = <<"Productivity">>,
            description = <<"Prioritize by urgency and importance.">>,
            key_insight = <<"Important rarely equals urgent">>,
            application = <<"Task prioritization, time management, delegation">>,
            failure_modes = [<<"Urgency addiction">>, <<"Neglecting important">>],
            keywords = [<<"eisenhower">>, <<"urgent">>, <<"important">>, <<"priority">>, <<"matrix">>]
        },
        #model{
            id = <<"deep_work">>,
            name = <<"Deep Work">>,
            category = <<"Productivity">>,
            description = <<"Focused, uninterrupted work on cognitively demanding tasks.">>,
            key_insight = <<"Shallow work is the enemy of excellence">>,
            application = <<"Knowledge work, creativity, skill development">>,
            failure_modes = [<<"Isolation">>, <<"Missing urgent issues">>],
            keywords = [<<"deep">>, <<"work">>, <<"focus">>, <<"concentration">>, <<"distraction">>]
        },
        #model{
            id = <<"batching">>,
            name = <<"Batching">>,
            category = <<"Productivity">>,
            description = <<"Group similar tasks together to reduce context switching.">>,
            key_insight = <<"Context switching is expensive">>,
            application = <<"Email, meetings, administrative tasks">>,
            failure_modes = [<<"Delayed responses">>, <<"Rigidity">>],
            keywords = [<<"batch">>, <<"group">>, <<"context">>, <<"switch">>, <<"efficiency">>]
        },
        #model{
            id = <<"time_blocking">>,
            name = <<"Time Blocking">>,
            category = <<"Productivity">>,
            description = <<"Schedule specific blocks of time for specific activities.">>,
            key_insight = <<"What gets scheduled gets done">>,
            application = <<"Calendar management, focus, work-life balance">>,
            failure_modes = [<<"Over-scheduling">>, <<"Inflexibility">>],
            keywords = [<<"time">>, <<"block">>, <<"schedule">>, <<"calendar">>, <<"plan">>]
        },

        %% ============================================
        %% NEGOTIATION & PERSUASION
        %% ============================================
        #model{
            id = <<"batna">>,
            name = <<"BATNA">>,
            category = <<"Negotiation">>,
            description = <<"Best Alternative To Negotiated Agreement - your walkaway option.">>,
            key_insight = <<"Power comes from alternatives">>,
            application = <<"Negotiation, job offers, contracts">>,
            failure_modes = [<<"Overestimating BATNA">>, <<"Revealing too much">>],
            keywords = [<<"batna">>, <<"alternative">>, <<"walkaway">>, <<"power">>, <<"leverage">>]
        },
        #model{
            id = <<"anchoring_negotiation">>,
            name = <<"Anchoring (Negotiation)">>,
            category = <<"Negotiation">>,
            description = <<"The first number sets the range for negotiation.">>,
            key_insight = <<"Anchor first when you have information advantage">>,
            application = <<"Salary negotiation, sales, contracts">>,
            failure_modes = [<<"Unrealistic anchors">>, <<"Losing credibility">>],
            keywords = [<<"anchor">>, <<"first">>, <<"offer">>, <<"range">>, <<"starting">>]
        },
        #model{
            id = <<"win_win">>,
            name = <<"Win-Win Negotiation">>,
            category = <<"Negotiation">>,
            description = <<"Expand the pie before dividing it.">>,
            key_insight = <<"Integrative deals create more value than distributive ones">>,
            application = <<"Business partnerships, conflict resolution, deals">>,
            failure_modes = [<<"Naive cooperation">>, <<"Exploitation">>],
            keywords = [<<"win-win">>, <<"integrative">>, <<"expand">>, <<"value">>, <<"mutual">>]
        },
        #model{
            id = <<"labeling">>,
            name = <<"Labeling">>,
            category = <<"Negotiation">>,
            description = <<"Name the other party's emotions to defuse them.">>,
            key_insight = <<"Acknowledged emotions lose their power">>,
            application = <<"Conflict resolution, sales, difficult conversations">>,
            failure_modes = [<<"Wrong labels">>, <<"Manipulation perception">>],
            keywords = [<<"label">>, <<"emotion">>, <<"acknowledge">>, <<"defuse">>, <<"empathy">>]
        },
        #model{
            id = <<"mirroring">>,
            name = <<"Mirroring">>,
            category = <<"Negotiation">>,
            description = <<"Repeat the last few words to encourage elaboration.">>,
            key_insight = <<"People love to hear themselves">>,
            application = <<"Information gathering, rapport building, interviews">>,
            failure_modes = [<<"Obvious technique">>, <<"Annoying repetition">>],
            keywords = [<<"mirror">>, <<"repeat">>, <<"rapport">>, <<"elaborate">>, <<"listen">>]
        },

        %% ============================================
        %% RISK & UNCERTAINTY
        %% ============================================
        #model{
            id = <<"black_swan">>,
            name = <<"Black Swan">>,
            category = <<"Risk">>,
            description = <<"Rare, unpredictable events with massive impact.">>,
            key_insight = <<"The biggest risks are the ones we don't see coming">>,
            application = <<"Risk management, portfolio design, scenario planning">>,
            failure_modes = [<<"Paranoia">>, <<"Inaction">>],
            keywords = [<<"black swan">>, <<"rare">>, <<"unpredictable">>, <<"impact">>, <<"taleb">>]
        },
        #model{
            id = <<"ergodicity">>,
            name = <<"Ergodicity">>,
            category = <<"Risk">>,
            description = <<"Time averages don't always equal ensemble averages.">>,
            key_insight = <<"Ruin changes everything - you can't recover from zero">>,
            application = <<"Betting, investing, career decisions">>,
            failure_modes = [<<"Over-caution">>, <<"Missing expected value">>],
            keywords = [<<"ergodicity">>, <<"ruin">>, <<"average">>, <<"time">>, <<"ensemble">>]
        },
        #model{
            id = <<"precautionary_principle">>,
            name = <<"Precautionary Principle">>,
            category = <<"Risk">>,
            description = <<"When stakes are high and irreversible, err on the side of caution.">>,
            key_insight = <<"Some risks aren't worth taking at any odds">>,
            application = <<"Policy, safety, existential risk">>,
            failure_modes = [<<"Paralysis">>, <<"Blocking progress">>],
            keywords = [<<"precaution">>, <<"caution">>, <<"irreversible">>, <<"stakes">>, <<"safety">>]
        },
        #model{
            id = <<"expected_value">>,
            name = <<"Expected Value">>,
            category = <<"Risk">>,
            description = <<"Probability-weighted average of all possible outcomes.">>,
            key_insight = <<"Focus on expected value, not individual outcomes">>,
            application = <<"Gambling, investing, decision-making">>,
            failure_modes = [<<"Ignoring variance">>, <<"Ruin risk">>],
            keywords = [<<"expected">>, <<"value">>, <<"probability">>, <<"outcome">>, <<"average">>]
        },
        #model{
            id = <<"kelly_criterion">>,
            name = <<"Kelly Criterion">>,
            category = <<"Risk">>,
            description = <<"Optimal bet sizing based on edge and odds.">>,
            key_insight = <<"Bet size matters as much as bet selection">>,
            application = <<"Investing, gambling, resource allocation">>,
            failure_modes = [<<"Overestimating edge">>, <<"Full Kelly volatility">>],
            keywords = [<<"kelly">>, <<"bet">>, <<"size">>, <<"optimal">>, <<"edge">>]
        },

        %% ============================================
        %% MUNGER'S PSYCHOLOGY OF HUMAN MISJUDGMENT
        %% ============================================
        #model{
            id = <<"reward_punishment_superresponse">>,
            name = <<"Reward/Punishment Superresponse">>,
            category = <<"Munger Psychology">>,
            description = <<"Incentives drive behavior more powerfully than we realize.">>,
            key_insight = <<"Never think about something else when you should be thinking about incentives">>,
            application = <<"Compensation design, behavior change, system design">>,
            failure_modes = [<<"Gaming">>, <<"Unintended consequences">>],
            keywords = [<<"incentive">>, <<"reward">>, <<"punishment">>, <<"behavior">>, <<"motivation">>]
        },
        #model{
            id = <<"doubt_avoidance">>,
            name = <<"Doubt-Avoidance Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"The brain quickly eliminates doubt by reaching decisions.">>,
            key_insight = <<"Rushed decisions often serve to reduce discomfort, not find truth">>,
            application = <<"Decision-making, hiring, negotiations">>,
            failure_modes = [<<"Premature closure">>, <<"Snap judgments">>],
            keywords = [<<"doubt">>, <<"avoidance">>, <<"decision">>, <<"quick">>, <<"closure">>]
        },
        #model{
            id = <<"inconsistency_avoidance">>,
            name = <<"Inconsistency-Avoidance Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"The brain resists changing conclusions and habits.">>,
            key_insight = <<"First conclusions become anchors that resist change">>,
            application = <<"Change management, habit breaking, belief updating">>,
            failure_modes = [<<"Stubbornness">>, <<"Outdated beliefs">>],
            keywords = [<<"inconsistency">>, <<"change">>, <<"habit">>, <<"resist">>, <<"anchor">>]
        },
        #model{
            id = <<"curiosity_tendency">>,
            name = <<"Curiosity Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"Humans have an innate drive to understand and learn.">>,
            key_insight = <<"Curiosity is a superpower that compounds knowledge">>,
            application = <<"Learning, innovation, problem-solving">>,
            failure_modes = [<<"Distraction">>, <<"Information overload">>],
            keywords = [<<"curiosity">>, <<"learn">>, <<"understand">>, <<"explore">>, <<"discover">>]
        },
        #model{
            id = <<"kantian_fairness">>,
            name = <<"Kantian Fairness Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"Humans expect fair exchanges and reciprocity.">>,
            key_insight = <<"Perceived unfairness triggers strong negative reactions">>,
            application = <<"Pricing, negotiations, organizational design">>,
            failure_modes = [<<"Exploitation">>, <<"Revenge">>],
            keywords = [<<"fairness">>, <<"reciprocity">>, <<"exchange">>, <<"justice">>, <<"equal">>]
        },
        #model{
            id = <<"envy_jealousy">>,
            name = <<"Envy/Jealousy Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"Humans feel pain at others' good fortune.">>,
            key_insight = <<"Comparison is the thief of joy">>,
            application = <<"Team dynamics, compensation, social media">>,
            failure_modes = [<<"Destructive competition">>, <<"Sabotage">>],
            keywords = [<<"envy">>, <<"jealousy">>, <<"comparison">>, <<"resentment">>]
        },
        #model{
            id = <<"contrast_misreaction">>,
            name = <<"Contrast-Misreaction Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"We judge things relative to nearby comparisons, not absolute value.">>,
            key_insight = <<"Context changes perception more than reality">>,
            application = <<"Pricing, negotiation, decision-making">>,
            failure_modes = [<<"Manipulation">>, <<"Poor absolute judgment">>],
            keywords = [<<"contrast">>, <<"comparison">>, <<"relative">>, <<"context">>, <<"anchor">>]
        },
        #model{
            id = <<"stress_influence">>,
            name = <<"Stress-Influence Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"Stress amplifies other psychological tendencies.">>,
            key_insight = <<"Under pressure, biases intensify">>,
            application = <<"Crisis management, high-stakes decisions, performance">>,
            failure_modes = [<<"Panic decisions">>, <<"Tunnel vision">>],
            keywords = [<<"stress">>, <<"pressure">>, <<"amplify">>, <<"crisis">>, <<"intensity">>]
        },
        #model{
            id = <<"association_tendency">>,
            name = <<"Association Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"We connect unrelated things that occur together.">>,
            key_insight = <<"Correlation feels like causation to the brain">>,
            application = <<"Branding, superstition, pattern recognition">>,
            failure_modes = [<<"False causation">>, <<"Superstition">>],
            keywords = [<<"association">>, <<"correlation">>, <<"connection">>, <<"pattern">>, <<"link">>]
        },
        #model{
            id = <<"simple_pain_avoiding">>,
            name = <<"Simple Pain-Avoiding Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"We avoid psychological pain through denial and distortion.">>,
            key_insight = <<"Reality denial is a defense mechanism">>,
            application = <<"Feedback, self-improvement, difficult conversations">>,
            failure_modes = [<<"Denial">>, <<"Avoidance">>],
            keywords = [<<"pain">>, <<"avoid">>, <<"denial">>, <<"distortion">>, <<"defense">>]
        },
        #model{
            id = <<"excessive_self_regard">>,
            name = <<"Excessive Self-Regard Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"We overvalue ourselves and our possessions.">>,
            key_insight = <<"Everyone thinks they're above average">>,
            application = <<"Hiring, self-assessment, negotiations">>,
            failure_modes = [<<"Overconfidence">>, <<"Endowment effect">>],
            keywords = [<<"self-regard">>, <<"overconfidence">>, <<"endowment">>, <<"ego">>, <<"bias">>]
        },
        #model{
            id = <<"over_optimism">>,
            name = <<"Over-Optimism Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"We systematically overestimate positive outcomes.">>,
            key_insight = <<"Hope is not a strategy">>,
            application = <<"Planning, forecasting, risk assessment">>,
            failure_modes = [<<"Underestimating risks">>, <<"Planning fallacy">>],
            keywords = [<<"optimism">>, <<"overestimate">>, <<"hope">>, <<"positive">>, <<"bias">>]
        },
        #model{
            id = <<"deprival_superreaction">>,
            name = <<"Deprival-Superreaction Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"Losing something hurts more than gaining it feels good.">>,
            key_insight = <<"Threatened losses trigger irrational behavior">>,
            application = <<"Change management, negotiations, marketing">>,
            failure_modes = [<<"Irrational resistance">>, <<"Sunk cost fallacy">>],
            keywords = [<<"deprival">>, <<"loss">>, <<"reaction">>, <<"threatened">>, <<"aversion">>]
        },
        #model{
            id = <<"senescence_misinfluence">>,
            name = <<"Senescence-Misinfluence Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"Cognitive abilities decline with age.">>,
            key_insight = <<"Continuous learning slows cognitive decline">>,
            application = <<"Succession planning, lifelong learning, self-awareness">>,
            failure_modes = [<<"Denial of decline">>, <<"Outdated thinking">>],
            keywords = [<<"aging">>, <<"decline">>, <<"cognitive">>, <<"learning">>, <<"senescence">>]
        },
        #model{
            id = <<"twaddle_tendency">>,
            name = <<"Twaddle Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"Humans talk about things they don't understand.">>,
            key_insight = <<"Silence is often wiser than speech">>,
            application = <<"Meetings, expertise assessment, communication">>,
            failure_modes = [<<"Wasted time">>, <<"Misinformation">>],
            keywords = [<<"twaddle">>, <<"nonsense">>, <<"talk">>, <<"expertise">>, <<"silence">>]
        },
        #model{
            id = <<"reason_respecting">>,
            name = <<"Reason-Respecting Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"People comply more when given a reason, even a bad one.">>,
            key_insight = <<"'Because' is a magic word">>,
            application = <<"Persuasion, leadership, compliance">>,
            failure_modes = [<<"Manipulation">>, <<"Weak reasoning">>],
            keywords = [<<"reason">>, <<"because">>, <<"comply">>, <<"explain">>, <<"justify">>]
        },
        #model{
            id = <<"lollapalooza">>,
            name = <<"Lollapalooza Effect">>,
            category = <<"Munger Psychology">>,
            description = <<"Multiple psychological tendencies acting together create extreme outcomes.">>,
            key_insight = <<"Confluence of factors creates outsized results">>,
            application = <<"Cult analysis, market bubbles, extreme behavior">>,
            failure_modes = [<<"Underestimating combinations">>, <<"Missing interactions">>],
            keywords = [<<"lollapalooza">>, <<"confluence">>, <<"combination">>, <<"extreme">>, <<"compound">>]
        }
    ],
    
    lists:foreach(fun(Model) ->
        ets:insert(models, Model)
    end, Models).
