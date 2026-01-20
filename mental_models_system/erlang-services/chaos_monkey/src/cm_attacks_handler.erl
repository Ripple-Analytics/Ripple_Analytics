%%%-------------------------------------------------------------------
%%% @doc Attacks Handler for Chaos Monkey
%%% 
%%% Lists available attack types.
%%% @end
%%%-------------------------------------------------------------------
-module(cm_attacks_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Attacks = [
        #{
            <<"id">> => <<"kill_container">>,
            <<"name">> => <<"Kill Container">>,
            <<"description">> => <<"Randomly kills a target container to test supervisor recovery">>,
            <<"severity">> => <<"high">>,
            <<"recovery_expected">> => true
        },
        #{
            <<"id">> => <<"network_latency">>,
            <<"name">> => <<"Network Latency">>,
            <<"description">> => <<"Injects 100-500ms network latency for 10 seconds">>,
            <<"severity">> => <<"medium">>,
            <<"recovery_expected">> => true
        },
        #{
            <<"id">> => <<"cpu_stress">>,
            <<"name">> => <<"CPU Stress">>,
            <<"description">> => <<"Stresses CPU in a container for 10 seconds">>,
            <<"severity">> => <<"medium">>,
            <<"recovery_expected">> => true
        },
        #{
            <<"id">> => <<"memory_pressure">>,
            <<"name">> => <<"Memory Pressure">>,
            <<"description">> => <<"Creates 50-150MB memory pressure for 10 seconds">>,
            <<"severity">> => <<"medium">>,
            <<"recovery_expected">> => true
        },
        #{
            <<"id">> => <<"service_restart">>,
            <<"name">> => <<"Service Restart">>,
            <<"description">> => <<"Gracefully restarts a random service">>,
            <<"severity">> => <<"low">>,
            <<"recovery_expected">> => true
        },
        #{
            <<"id">> => <<"random">>,
            <<"name">> => <<"Random Attack">>,
            <<"description">> => <<"Executes a randomly selected attack type">>,
            <<"severity">> => <<"variable">>,
            <<"recovery_expected">> => true
        }
    ],
    
    Response = #{
        <<"attacks">> => Attacks,
        <<"count">> => length(Attacks)
    },
    
    Req = cowboy_req:reply(200, 
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>},
        jsx:encode(Response), Req0),
    {ok, Req, State}.
