%%%-------------------------------------------------------------------
%%% @doc Model Data - SYSTEMS THINKING
%%% @end
%%%-------------------------------------------------------------------
-module(model_data_systems_thinking).

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
        }
    ].
