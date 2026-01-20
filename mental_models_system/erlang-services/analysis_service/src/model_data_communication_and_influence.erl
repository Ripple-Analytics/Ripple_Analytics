%%%-------------------------------------------------------------------
%%% @doc Model Data - COMMUNICATION & INFLUENCE
%%% @end
%%%-------------------------------------------------------------------
-module(model_data_communication_and_influence).

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
            id = <<"framing">>,
            name = <<"Framing">>,
            category = <<"Communication">>,
            description = <<"How information is presented affects how it's perceived.">>,
            key_insight = <<"The frame changes the picture">>,
            application = <<"Negotiation, marketing, leadership">>,
            failure_modes = [<<"Manipulation">>, <<"Misrepresentation">>],
            keywords = [<<"frame">>, <<"present">>, <<"context">>, <<"perception">>, <<"spin">>]
        },
        #model{
            id = <<"storytelling">>,
            name = <<"Storytelling">>,
            category = <<"Communication">>,
            description = <<"Narratives are more memorable and persuasive than facts alone.">>,
            key_insight = <<"Stories bypass rational resistance">>,
            application = <<"Leadership, sales, teaching">>,
            failure_modes = [<<"Manipulation">>, <<"Oversimplification">>],
            keywords = [<<"story">>, <<"narrative">>, <<"persuade">>, <<"memorable">>, <<"engage">>]
        },
        #model{
            id = <<"steel_manning">>,
            name = <<"Steel Manning">>,
            category = <<"Communication">>,
            description = <<"Present the strongest version of opposing arguments.">>,
            key_insight = <<"Understanding opponents strengthens your position">>,
            application = <<"Debate, decision-making, conflict resolution">>,
            failure_modes = [<<"Wasting time">>, <<"Appearing weak">>],
            keywords = [<<"steel man">>, <<"argument">>, <<"opponent">>, <<"strongest">>, <<"fair">>]
        },
        #model{
            id = <<"socratic_method">>,
            name = <<"Socratic Method">>,
            category = <<"Communication">>,
            description = <<"Use questions to stimulate critical thinking and illuminate ideas.">>,
            key_insight = <<"Questions are more powerful than statements">>,
            application = <<"Teaching, coaching, discovery">>,
            failure_modes = [<<"Frustration">>, <<"Manipulation">>],
            keywords = [<<"socratic">>, <<"question">>, <<"inquiry">>, <<"think">>, <<"discover">>]
        }
    ].
