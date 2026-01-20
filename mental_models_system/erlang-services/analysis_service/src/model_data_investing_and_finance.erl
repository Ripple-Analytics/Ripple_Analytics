%%%-------------------------------------------------------------------
%%% @doc Model Data - INVESTING & FINANCE
%%% @end
%%%-------------------------------------------------------------------
-module(model_data_investing_and_finance).

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
            id = <<"mr_market">>,
            name = <<"Mr. Market">>,
            category = <<"Investing">>,
            description = <<"The market is an emotional partner offering prices, not a guide to value.">>,
            key_insight = <<"Market prices reflect sentiment, not always value">>,
            application = <<"Value investing, emotional discipline, contrarian thinking">>,
            failure_modes = [<<"Ignoring market signals">>, <<"Stubbornness">>],
            keywords = [<<"market">>, <<"emotion">>, <<"value">>, <<"price">>, <<"sentiment">>]
        },
        #model{
            id = <<"moat">>,
            name = <<"Economic Moat">>,
            category = <<"Investing">>,
            description = <<"Sustainable competitive advantages that protect profits.">>,
            key_insight = <<"Durable advantages compound over time">>,
            application = <<"Business analysis, competitive strategy, investment selection">>,
            failure_modes = [<<"Moat erosion">>, <<"Disruption">>],
            keywords = [<<"moat">>, <<"competitive">>, <<"advantage">>, <<"durable">>, <<"protect">>]
        },
        #model{
            id = <<"intrinsic_value">>,
            name = <<"Intrinsic Value">>,
            category = <<"Investing">>,
            description = <<"The true worth of an asset based on fundamentals, not market price.">>,
            key_insight = <<"Price is what you pay; value is what you get">>,
            application = <<"Investment analysis, negotiation, business valuation">>,
            failure_modes = [<<"Miscalculation">>, <<"Changing fundamentals">>],
            keywords = [<<"intrinsic">>, <<"value">>, <<"fundamental">>, <<"worth">>, <<"price">>]
        },
        #model{
            id = <<"circle_of_competence_investing">>,
            name = <<"Circle of Competence (Investing)">>,
            category = <<"Investing">>,
            description = <<"Only invest in businesses you truly understand.">>,
            key_insight = <<"Knowing what you don't know prevents costly mistakes">>,
            application = <<"Portfolio construction, due diligence, risk management">>,
            failure_modes = [<<"Overconfidence">>, <<"Missed opportunities">>],
            keywords = [<<"competence">>, <<"understand">>, <<"knowledge">>, <<"expertise">>]
        },
        #model{
            id = <<"skin_in_game">>,
            name = <<"Skin in the Game">>,
            category = <<"Investing">>,
            description = <<"Having personal risk aligned with outcomes improves decision quality.">>,
            key_insight = <<"People behave differently when they bear consequences">>,
            application = <<"Governance, incentive design, trust assessment">>,
            failure_modes = [<<"Risk aversion">>, <<"Short-termism">>],
            keywords = [<<"skin">>, <<"game">>, <<"risk">>, <<"aligned">>, <<"consequences">>]
        }
    ].
