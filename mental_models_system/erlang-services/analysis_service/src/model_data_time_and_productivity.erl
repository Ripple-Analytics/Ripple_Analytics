%%%-------------------------------------------------------------------
%%% @doc Model Data - TIME & PRODUCTIVITY
%%% @end
%%%-------------------------------------------------------------------
-module(model_data_time_and_productivity).

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
            id = <<"parkinsons_law">>,
            name = <<"Parkinson's Law">>,
            category = <<"Productivity">>,
            description = <<"Work expands to fill the time available for completion.">>,
            key_insight = <<"Deadlines create focus">>,
            application = <<"Time management, project planning, productivity">>,
            failure_modes = [<<"Rushed work">>, <<"Burnout">>],
            keywords = [<<"parkinson">>, <<"time">>, <<"deadline">>, <<"expand">>, <<"fill">>]
        },
        #model{
            id = <<"eisenhower_matrix">>,
            name = <<"Eisenhower Matrix">>,
            category = <<"Productivity">>,
            description = <<"Prioritize by urgency and importance.">>,
            key_insight = <<"Important rarely equals urgent">>,
            application = <<"Task prioritization, time management, delegation">>,
            failure_modes = [<<"Urgency addiction">>, <<"Neglecting important">>],
            keywords = [<<"eisenhower">>, <<"urgent">>, <<"important">>, <<"priority">>, <<"matrix">>]
        },
        #model{
            id = <<"deep_work">>,
            name = <<"Deep Work">>,
            category = <<"Productivity">>,
            description = <<"Focused, uninterrupted work on cognitively demanding tasks.">>,
            key_insight = <<"Shallow work is the enemy of excellence">>,
            application = <<"Knowledge work, creativity, skill development">>,
            failure_modes = [<<"Isolation">>, <<"Missing urgent issues">>],
            keywords = [<<"deep">>, <<"work">>, <<"focus">>, <<"concentration">>, <<"distraction">>]
        },
        #model{
            id = <<"batching">>,
            name = <<"Batching">>,
            category = <<"Productivity">>,
            description = <<"Group similar tasks together to reduce context switching.">>,
            key_insight = <<"Context switching is expensive">>,
            application = <<"Email, meetings, administrative tasks">>,
            failure_modes = [<<"Delayed responses">>, <<"Rigidity">>],
            keywords = [<<"batch">>, <<"group">>, <<"context">>, <<"switch">>, <<"efficiency">>]
        },
        #model{
            id = <<"time_blocking">>,
            name = <<"Time Blocking">>,
            category = <<"Productivity">>,
            description = <<"Schedule specific blocks of time for specific activities.">>,
            key_insight = <<"What gets scheduled gets done">>,
            application = <<"Calendar management, focus, work-life balance">>,
            failure_modes = [<<"Over-scheduling">>, <<"Inflexibility">>],
            keywords = [<<"time">>, <<"block">>, <<"schedule">>, <<"calendar">>, <<"plan">>]
        }
    ].
