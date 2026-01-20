%%%-------------------------------------------------------------------
%%% @doc Model Data - LEARNING & KNOWLEDGE
%%% @end
%%%-------------------------------------------------------------------
-module(model_data_learning_and_knowledge).

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
        }
    ].
