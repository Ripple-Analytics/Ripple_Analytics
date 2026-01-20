%%%-------------------------------------------------------------------
%%% @doc Model Data - LEADERSHIP & MANAGEMENT
%%% @end
%%%-------------------------------------------------------------------
-module(model_data_leadership_and_management).

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
            id = <<"servant_leadership">>,
            name = <<"Servant Leadership">>,
            category = <<"Leadership">>,
            description = <<"Leaders exist to serve their teams, not the reverse.">>,
            key_insight = <<"The best leaders put others first">>,
            application = <<"Team management, organizational culture, coaching">>,
            failure_modes = [<<"Doormat syndrome">>, <<"Lack of direction">>],
            keywords = [<<"servant">>, <<"leadership">>, <<"serve">>, <<"team">>, <<"support">>]
        },
        #model{
            id = <<"delegation">>,
            name = <<"Delegation">>,
            category = <<"Leadership">>,
            description = <<"Assigning responsibility and authority to others.">>,
            key_insight = <<"You can delegate authority but not responsibility">>,
            application = <<"Management, scaling, team development">>,
            failure_modes = [<<"Micromanagement">>, <<"Abdication">>],
            keywords = [<<"delegate">>, <<"authority">>, <<"responsibility">>, <<"empower">>, <<"trust">>]
        },
        #model{
            id = <<"radical_candor">>,
            name = <<"Radical Candor">>,
            category = <<"Leadership">>,
            description = <<"Care personally while challenging directly.">>,
            key_insight = <<"Kindness without honesty is manipulation">>,
            application = <<"Feedback, coaching, team development">>,
            failure_modes = [<<"Obnoxious aggression">>, <<"Ruinous empathy">>],
            keywords = [<<"candor">>, <<"feedback">>, <<"honest">>, <<"care">>, <<"direct">>]
        },
        #model{
            id = <<"high_output_management">>,
            name = <<"High Output Management">>,
            category = <<"Leadership">>,
            description = <<"A manager's output is the output of their team.">>,
            key_insight = <<"Leverage your time through others">>,
            application = <<"Management, productivity, team building">>,
            failure_modes = [<<"Individual contributor mindset">>, <<"Neglecting team">>],
            keywords = [<<"output">>, <<"management">>, <<"team">>, <<"leverage">>, <<"grove">>]
        },
        #model{
            id = <<"mission_command">>,
            name = <<"Mission Command">>,
            category = <<"Leadership">>,
            description = <<"Give clear intent and let people figure out how to achieve it.">>,
            key_insight = <<"Decentralized execution with centralized intent">>,
            application = <<"Military, startups, distributed teams">>,
            failure_modes = [<<"Unclear intent">>, <<"Lack of trust">>],
            keywords = [<<"mission">>, <<"command">>, <<"intent">>, <<"autonomy">>, <<"decentralized">>]
        }
    ].
