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
        }
    ],
    
    lists:foreach(fun(Model) ->
        ets:insert(models, Model)
    end, Models).
