%%%-------------------------------------------------------------------
%%% @doc Model Data - ECONOMICS & MARKETS
%%% @end
%%%-------------------------------------------------------------------
-module(model_data_economics_and_markets).

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
        }
    ].
