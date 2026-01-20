%%%-------------------------------------------------------------------
%%% @doc Model Data - RISK & UNCERTAINTY
%%% @end
%%%-------------------------------------------------------------------
-module(model_data_risk_and_uncertainty_part2).

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
            id = <<"excessive_self_regard">>,
            name = <<"Excessive Self-Regard Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"We overvalue ourselves and our possessions.">>,
            key_insight = <<"Everyone thinks they're above average">>,
            application = <<"Hiring, self-assessment, negotiations">>,
            failure_modes = [<<"Overconfidence">>, <<"Endowment effect">>],
            keywords = [<<"self-regard">>, <<"overconfidence">>, <<"endowment">>, <<"ego">>, <<"bias">>]
        },
        #model{
            id = <<"over_optimism">>,
            name = <<"Over-Optimism Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"We systematically overestimate positive outcomes.">>,
            key_insight = <<"Hope is not a strategy">>,
            application = <<"Planning, forecasting, risk assessment">>,
            failure_modes = [<<"Underestimating risks">>, <<"Planning fallacy">>],
            keywords = [<<"optimism">>, <<"overestimate">>, <<"hope">>, <<"positive">>, <<"bias">>]
        },
        #model{
            id = <<"deprival_superreaction">>,
            name = <<"Deprival-Superreaction Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"Losing something hurts more than gaining it feels good.">>,
            key_insight = <<"Threatened losses trigger irrational behavior">>,
            application = <<"Change management, negotiations, marketing">>,
            failure_modes = [<<"Irrational resistance">>, <<"Sunk cost fallacy">>],
            keywords = [<<"deprival">>, <<"loss">>, <<"reaction">>, <<"threatened">>, <<"aversion">>]
        },
        #model{
            id = <<"senescence_misinfluence">>,
            name = <<"Senescence-Misinfluence Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"Cognitive abilities decline with age.">>,
            key_insight = <<"Continuous learning slows cognitive decline">>,
            application = <<"Succession planning, lifelong learning, self-awareness">>,
            failure_modes = [<<"Denial of decline">>, <<"Outdated thinking">>],
            keywords = [<<"aging">>, <<"decline">>, <<"cognitive">>, <<"learning">>, <<"senescence">>]
        },
        #model{
            id = <<"twaddle_tendency">>,
            name = <<"Twaddle Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"Humans talk about things they don't understand.">>,
            key_insight = <<"Silence is often wiser than speech">>,
            application = <<"Meetings, expertise assessment, communication">>,
            failure_modes = [<<"Wasted time">>, <<"Misinformation">>],
            keywords = [<<"twaddle">>, <<"nonsense">>, <<"talk">>, <<"expertise">>, <<"silence">>]
        },
        #model{
            id = <<"reason_respecting">>,
            name = <<"Reason-Respecting Tendency">>,
            category = <<"Munger Psychology">>,
            description = <<"People comply more when given a reason, even a bad one.">>,
            key_insight = <<"'Because' is a magic word">>,
            application = <<"Persuasion, leadership, compliance">>,
            failure_modes = [<<"Manipulation">>, <<"Weak reasoning">>],
            keywords = [<<"reason">>, <<"because">>, <<"comply">>, <<"explain">>, <<"justify">>]
        },
        #model{
            id = <<"lollapalooza">>,
            name = <<"Lollapalooza Effect">>,
            category = <<"Munger Psychology">>,
            description = <<"Multiple psychological tendencies acting together create extreme outcomes.">>,
            key_insight = <<"Confluence of factors creates outsized results">>,
            application = <<"Cult analysis, market bubbles, extreme behavior">>,
            failure_modes = [<<"Underestimating combinations">>, <<"Missing interactions">>],
            keywords = [<<"lollapalooza">>, <<"confluence">>, <<"combination">>, <<"extreme">>, <<"compound">>]
        }
    ].
