%%%-------------------------------------------------------------------
%%% @doc Model Data - MATHEMATICS & STATISTICS
%%% @end
%%%-------------------------------------------------------------------
-module(model_data_mathematics_and_statistics).

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
            id = <<"power_law">>,
            name = <<"Power Law">>,
            category = <<"Mathematics">>,
            description = <<"A few items account for most of the effect (80/20 rule).">>,
            key_insight = <<"Distributions are often extremely unequal">>,
            application = <<"Resource allocation, prioritization, venture capital">>,
            failure_modes = [<<"Ignoring the tail">>, <<"Over-concentration">>],
            keywords = [<<"power law">>, <<"pareto">>, <<"80/20">>, <<"distribution">>, <<"inequality">>]
        },
        #model{
            id = <<"regression_to_mean">>,
            name = <<"Regression to the Mean">>,
            category = <<"Mathematics">>,
            description = <<"Extreme results tend to be followed by more average ones.">>,
            key_insight = <<"Exceptional performance often reverts to normal">>,
            application = <<"Performance evaluation, sports analysis, investment returns">>,
            failure_modes = [<<"Overreacting to outliers">>, <<"False patterns">>],
            keywords = [<<"regression">>, <<"mean">>, <<"average">>, <<"reversion">>, <<"normal">>]
        },
        #model{
            id = <<"bayes_theorem">>,
            name = <<"Bayes' Theorem">>,
            category = <<"Mathematics">>,
            description = <<"Update probability estimates based on new evidence.">>,
            key_insight = <<"Prior beliefs should be updated rationally with new information">>,
            application = <<"Medical diagnosis, spam filtering, decision-making">>,
            failure_modes = [<<"Ignoring base rates">>, <<"Overweighting new evidence">>],
            keywords = [<<"bayes">>, <<"probability">>, <<"update">>, <<"prior">>, <<"evidence">>]
        },
        #model{
            id = <<"law_of_large_numbers">>,
            name = <<"Law of Large Numbers">>,
            category = <<"Mathematics">>,
            description = <<"As sample size increases, results approach the expected value.">>,
            key_insight = <<"Small samples are unreliable; large samples reveal truth">>,
            application = <<"Insurance, gambling, A/B testing">>,
            failure_modes = [<<"Small sample fallacy">>, <<"Gambler's fallacy">>],
            keywords = [<<"large numbers">>, <<"sample">>, <<"average">>, <<"expected">>, <<"converge">>]
        },
        #model{
            id = <<"compounding">>,
            name = <<"Compounding">>,
            category = <<"Mathematics">>,
            description = <<"Growth on growth creates exponential results over time.">>,
            key_insight = <<"Small consistent gains become enormous over time">>,
            application = <<"Investing, learning, relationship building">>,
            failure_modes = [<<"Impatience">>, <<"Interrupting the process">>],
            keywords = [<<"compound">>, <<"exponential">>, <<"growth">>, <<"interest">>, <<"accumulate">>]
        },
        #model{
            id = <<"normal_distribution">>,
            name = <<"Normal Distribution">>,
            category = <<"Mathematics">>,
            description = <<"Many phenomena cluster around an average with predictable spread.">>,
            key_insight = <<"Most outcomes are average; extremes are rare">>,
            application = <<"Quality control, grading, risk assessment">>,
            failure_modes = [<<"Assuming normality">>, <<"Fat tail blindness">>],
            keywords = [<<"normal">>, <<"bell curve">>, <<"gaussian">>, <<"average">>, <<"standard deviation">>]
        },
        #model{
            id = <<"fat_tails">>,
            name = <<"Fat Tails">>,
            category = <<"Mathematics">>,
            description = <<"Extreme events occur more often than normal distribution predicts.">>,
            key_insight = <<"Black swans are more common than we think">>,
            application = <<"Risk management, financial modeling, disaster planning">>,
            failure_modes = [<<"Underestimating extremes">>, <<"Model failure">>],
            keywords = [<<"fat tails">>, <<"black swan">>, <<"extreme">>, <<"outlier">>, <<"risk">>]
        }
    ].
