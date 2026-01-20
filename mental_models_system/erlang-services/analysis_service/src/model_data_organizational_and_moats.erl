%%%-------------------------------------------------------------------
%%% @doc Model Data - ORGANIZATIONAL & MOATS
%%% @end
%%%-------------------------------------------------------------------
-module(model_data_organizational_and_moats).

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
            id = <<"economies_of_scale">>,
            name = <<"Economies of Scale">>,
            category = <<"Organizational">>,
            description = <<"Cost per unit decreases as production volume increases.">>,
            key_insight = <<"Size creates cost advantages that compound">>,
            application = <<"Business strategy, pricing, competitive analysis">>,
            failure_modes = [<<"Diseconomies of scale">>, <<"Bureaucracy">>],
            keywords = [<<"scale">>, <<"economies">>, <<"volume">>, <<"cost">>, <<"size">>]
        },
        #model{
            id = <<"switching_costs">>,
            name = <<"Switching Costs">>,
            category = <<"Organizational">>,
            description = <<"The costs (time, money, effort) of changing from one product to another.">>,
            key_insight = <<"High switching costs create customer lock-in">>,
            application = <<"Product strategy, customer retention, competitive moats">>,
            failure_modes = [<<"Customer resentment">>, <<"Disruption vulnerability">>],
            keywords = [<<"switching">>, <<"cost">>, <<"lock-in">>, <<"change">>, <<"migrate">>]
        },
        #model{
            id = <<"brand">>,
            name = <<"Brand">>,
            category = <<"Organizational">>,
            description = <<"The accumulated perception and trust associated with a name or symbol.">>,
            key_insight = <<"Brand is a promise that reduces customer uncertainty">>,
            application = <<"Marketing, pricing power, competitive positioning">>,
            failure_modes = [<<"Brand dilution">>, <<"Reputation damage">>],
            keywords = [<<"brand">>, <<"reputation">>, <<"trust">>, <<"perception">>, <<"name">>]
        },
        #model{
            id = <<"principal_agent">>,
            name = <<"Principal-Agent Problem">>,
            category = <<"Organizational">>,
            description = <<"Conflicts arise when one party (agent) acts on behalf of another (principal).">>,
            key_insight = <<"Agents optimize for their own interests, not principals'">>,
            application = <<"Governance, compensation design, outsourcing">>,
            failure_modes = [<<"Moral hazard">>, <<"Adverse selection">>],
            keywords = [<<"principal">>, <<"agent">>, <<"conflict">>, <<"interest">>, <<"incentive">>]
        },
        #model{
            id = <<"bureaucracy">>,
            name = <<"Bureaucracy">>,
            category = <<"Organizational">>,
            description = <<"Administrative systems that prioritize rules and procedures over outcomes.">>,
            key_insight = <<"Bureaucracy trades efficiency for consistency and control">>,
            application = <<"Organizational design, process improvement, scaling">>,
            failure_modes = [<<"Red tape">>, <<"Innovation stifling">>],
            keywords = [<<"bureaucracy">>, <<"rules">>, <<"procedure">>, <<"administration">>, <<"process">>]
        }
    ].
