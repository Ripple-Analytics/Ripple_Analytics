%%%-------------------------------------------------------------------
%%% @doc Model Data - ECONOMICS & FINANCE
%%% @end
%%%-------------------------------------------------------------------
-module(model_data_economics_and_finance).

-export([get_models/0]).

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

get_models() ->
    [
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
        }
    ].
