%%%-------------------------------------------------------------------
%%% @doc Model Data - MILITARY & STRATEGY
%%% @end
%%%-------------------------------------------------------------------
-module(model_data_military_and_strategy).

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
            id = <<"asymmetric_warfare">>,
            name = <<"Asymmetric Warfare">>,
            category = <<"Military & Strategy">>,
            description = <<"Weaker parties use unconventional tactics against stronger opponents.">>,
            key_insight = <<"Don't fight on your opponent's terms">>,
            application = <<"Competitive strategy, startup tactics, negotiation">>,
            failure_modes = [<<"Escalation">>, <<"Retaliation">>],
            keywords = [<<"asymmetric">>, <<"guerrilla">>, <<"unconventional">>, <<"underdog">>]
        },
        #model{
            id = <<"flanking">>,
            name = <<"Flanking">>,
            category = <<"Military & Strategy">>,
            description = <<"Attack from the side where defenses are weakest.">>,
            key_insight = <<"Avoid frontal assaults on strength">>,
            application = <<"Market entry, competitive positioning, negotiation">>,
            failure_modes = [<<"Overextension">>, <<"Counter-flanking">>],
            keywords = [<<"flank">>, <<"side">>, <<"weakness">>, <<"indirect">>, <<"maneuver">>]
        },
        #model{
            id = <<"fog_of_war">>,
            name = <<"Fog of War">>,
            category = <<"Military & Strategy">>,
            description = <<"Uncertainty and incomplete information in complex situations.">>,
            key_insight = <<"Plans rarely survive first contact with reality">>,
            application = <<"Project planning, crisis management, decision-making">>,
            failure_modes = [<<"Paralysis">>, <<"Overconfidence">>],
            keywords = [<<"fog">>, <<"war">>, <<"uncertainty">>, <<"incomplete">>, <<"chaos">>]
        },
        #model{
            id = <<"force_multiplier">>,
            name = <<"Force Multiplier">>,
            category = <<"Military & Strategy">>,
            description = <<"Factors that dramatically increase effectiveness.">>,
            key_insight = <<"Some advantages compound your other advantages">>,
            application = <<"Team building, technology adoption, skill development">>,
            failure_modes = [<<"Over-reliance">>, <<"Single point of failure">>],
            keywords = [<<"force">>, <<"multiplier">>, <<"amplify">>, <<"effectiveness">>, <<"leverage">>]
        },
        #model{
            id = <<"schwerpunkt">>,
            name = <<"Schwerpunkt (Focus Point)">>,
            category = <<"Military & Strategy">>,
            description = <<"Concentrate overwhelming force at the decisive point.">>,
            key_insight = <<"Dispersion leads to defeat; concentration wins">>,
            application = <<"Resource allocation, product strategy, campaign planning">>,
            failure_modes = [<<"Wrong focus">>, <<"Neglecting other areas">>],
            keywords = [<<"focus">>, <<"concentrate">>, <<"decisive">>, <<"overwhelming">>, <<"priority">>]
        }
    ].
