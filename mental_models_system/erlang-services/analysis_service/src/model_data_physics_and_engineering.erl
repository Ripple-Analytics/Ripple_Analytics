%%%-------------------------------------------------------------------
%%% @doc Model Data - PHYSICS & ENGINEERING
%%% @end
%%%-------------------------------------------------------------------
-module(model_data_physics_and_engineering).

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
        }
    ].
