%%%-------------------------------------------------------------------
%%% @doc Model Data - BIOLOGY & EVOLUTION
%%% @end
%%%-------------------------------------------------------------------
-module(model_data_biology_and_evolution).

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
            id = <<"natural_selection">>,
            name = <<"Natural Selection">>,
            category = <<"Biology & Evolution">>,
            description = <<"Organisms better adapted to their environment tend to survive and reproduce.">>,
            key_insight = <<"Adaptation to environment determines survival">>,
            application = <<"Market competition, product evolution, organizational fitness">>,
            failure_modes = [<<"Maladaptation">>, <<"Environmental mismatch">>],
            keywords = [<<"selection">>, <<"adaptation">>, <<"survival">>, <<"fitness">>, <<"evolution">>]
        },
        #model{
            id = <<"red_queen_effect">>,
            name = <<"Red Queen Effect">>,
            category = <<"Biology & Evolution">>,
            description = <<"You must run faster just to stay in place as competitors evolve.">>,
            key_insight = <<"Standing still means falling behind">>,
            application = <<"Competitive strategy, continuous improvement, arms races">>,
            failure_modes = [<<"Exhaustion">>, <<"Resource depletion">>],
            keywords = [<<"red queen">>, <<"competition">>, <<"evolution">>, <<"race">>, <<"adapt">>]
        },
        #model{
            id = <<"adaptation">>,
            name = <<"Adaptation">>,
            category = <<"Biology & Evolution">>,
            description = <<"Organisms change to better fit their environment over time.">>,
            key_insight = <<"What works in one environment may fail in another">>,
            application = <<"Change management, market entry, personal development">>,
            failure_modes = [<<"Over-specialization">>, <<"Rigidity">>],
            keywords = [<<"adapt">>, <<"change">>, <<"environment">>, <<"fit">>, <<"evolve">>]
        },
        #model{
            id = <<"symbiosis">>,
            name = <<"Symbiosis">>,
            category = <<"Biology & Evolution">>,
            description = <<"Close relationships between different species that benefit one or both.">>,
            key_insight = <<"Cooperation can be more powerful than competition">>,
            application = <<"Partnerships, ecosystems, platform businesses">>,
            failure_modes = [<<"Parasitism">>, <<"Dependency">>],
            keywords = [<<"symbiosis">>, <<"mutualism">>, <<"cooperation">>, <<"partnership">>]
        },
        #model{
            id = <<"niche">>,
            name = <<"Ecological Niche">>,
            category = <<"Biology & Evolution">>,
            description = <<"The specific role and position an organism occupies in its ecosystem.">>,
            key_insight = <<"Specialization reduces competition">>,
            application = <<"Market positioning, career strategy, product differentiation">>,
            failure_modes = [<<"Niche collapse">>, <<"Over-specialization">>],
            keywords = [<<"niche">>, <<"specialization">>, <<"position">>, <<"role">>, <<"ecosystem">>]
        }
    ].
