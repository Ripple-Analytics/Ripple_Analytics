%%%-------------------------------------------------------------------
%%% @doc Model Data - RISK & UNCERTAINTY
%%% @end
%%%-------------------------------------------------------------------
-module(model_data_risk_and_uncertainty_part1).

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
        }
    ].
