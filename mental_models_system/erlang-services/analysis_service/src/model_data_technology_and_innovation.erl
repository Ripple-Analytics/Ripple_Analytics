%%%-------------------------------------------------------------------
%%% @doc Model Data - TECHNOLOGY & INNOVATION
%%% @end
%%%-------------------------------------------------------------------
-module(model_data_technology_and_innovation).

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
        }
    ].
