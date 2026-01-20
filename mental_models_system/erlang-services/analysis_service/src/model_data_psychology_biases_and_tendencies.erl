%%%-------------------------------------------------------------------
%%% @doc Model Data - PSYCHOLOGY: BIASES & TENDENCIES
%%% @end
%%%-------------------------------------------------------------------
-module(model_data_psychology_biases_and_tendencies).

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
        }
    ].
