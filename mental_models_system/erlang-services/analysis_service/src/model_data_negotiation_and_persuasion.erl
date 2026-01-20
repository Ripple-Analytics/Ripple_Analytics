%%%-------------------------------------------------------------------
%%% @doc Model Data - NEGOTIATION & PERSUASION
%%% @end
%%%-------------------------------------------------------------------
-module(model_data_negotiation_and_persuasion).

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
            id = <<"batna">>,
            name = <<"BATNA">>,
            category = <<"Negotiation">>,
            description = <<"Best Alternative To Negotiated Agreement - your walkaway option.">>,
            key_insight = <<"Power comes from alternatives">>,
            application = <<"Negotiation, job offers, contracts">>,
            failure_modes = [<<"Overestimating BATNA">>, <<"Revealing too much">>],
            keywords = [<<"batna">>, <<"alternative">>, <<"walkaway">>, <<"power">>, <<"leverage">>]
        },
        #model{
            id = <<"anchoring_negotiation">>,
            name = <<"Anchoring (Negotiation)">>,
            category = <<"Negotiation">>,
            description = <<"The first number sets the range for negotiation.">>,
            key_insight = <<"Anchor first when you have information advantage">>,
            application = <<"Salary negotiation, sales, contracts">>,
            failure_modes = [<<"Unrealistic anchors">>, <<"Losing credibility">>],
            keywords = [<<"anchor">>, <<"first">>, <<"offer">>, <<"range">>, <<"starting">>]
        },
        #model{
            id = <<"win_win">>,
            name = <<"Win-Win Negotiation">>,
            category = <<"Negotiation">>,
            description = <<"Expand the pie before dividing it.">>,
            key_insight = <<"Integrative deals create more value than distributive ones">>,
            application = <<"Business partnerships, conflict resolution, deals">>,
            failure_modes = [<<"Naive cooperation">>, <<"Exploitation">>],
            keywords = [<<"win-win">>, <<"integrative">>, <<"expand">>, <<"value">>, <<"mutual">>]
        },
        #model{
            id = <<"labeling">>,
            name = <<"Labeling">>,
            category = <<"Negotiation">>,
            description = <<"Name the other party's emotions to defuse them.">>,
            key_insight = <<"Acknowledged emotions lose their power">>,
            application = <<"Conflict resolution, sales, difficult conversations">>,
            failure_modes = [<<"Wrong labels">>, <<"Manipulation perception">>],
            keywords = [<<"label">>, <<"emotion">>, <<"acknowledge">>, <<"defuse">>, <<"empathy">>]
        },
        #model{
            id = <<"mirroring">>,
            name = <<"Mirroring">>,
            category = <<"Negotiation">>,
            description = <<"Repeat the last few words to encourage elaboration.">>,
            key_insight = <<"People love to hear themselves">>,
            application = <<"Information gathering, rapport building, interviews">>,
            failure_modes = [<<"Obvious technique">>, <<"Annoying repetition">>],
            keywords = [<<"mirror">>, <<"repeat">>, <<"rapport">>, <<"elaborate">>, <<"listen">>]
        }
    ].
