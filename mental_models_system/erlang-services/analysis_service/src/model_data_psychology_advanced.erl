%%%-------------------------------------------------------------------
%%% @doc Model Data - PSYCHOLOGY: ADVANCED
%%% @end
%%%-------------------------------------------------------------------
-module(model_data_psychology_advanced).

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
        }
    ].
