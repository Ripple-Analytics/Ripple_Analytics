%%%-------------------------------------------------------------------
%%% @doc Attack Handler for Chaos Monkey
%%% 
%%% Triggers specific attacks on demand.
%%% @end
%%%-------------------------------------------------------------------
-module(cm_attack_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    
    case Method of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            handle_attack(Body, Req1, State);
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_attack(Body, Req0, State) ->
    try jsx:decode(Body, [return_maps]) of
        Params ->
            AttackType = case maps:get(<<"type">>, Params, <<"random">>) of
                <<"kill_container">> -> kill_container;
                <<"network_latency">> -> network_latency;
                <<"cpu_stress">> -> cpu_stress;
                <<"memory_pressure">> -> memory_pressure;
                <<"service_restart">> -> service_restart;
                _ -> random
            end,
            
            Result = chaos_engine:execute_attack(AttackType),
            
            Response = case Result of
                {ok, Data} ->
                    #{<<"success">> => true, <<"attack">> => Data};
                {error, chaos_disabled} ->
                    #{<<"success">> => false, <<"error">> => <<"Chaos Monkey is disabled. Enable it first.">>};
                {error, Reason} ->
                    #{<<"success">> => false, <<"error">> => list_to_binary(io_lib:format("~p", [Reason]))}
            end,
            
            Req = cowboy_req:reply(200, cors_headers(), jsx:encode(Response), Req0),
            {ok, Req, State}
    catch
        _:_ ->
            %% If no body or invalid JSON, execute random attack
            Result = chaos_engine:execute_attack(random),
            
            Response = case Result of
                {ok, Data} ->
                    #{<<"success">> => true, <<"attack">> => Data};
                {error, chaos_disabled} ->
                    #{<<"success">> => false, <<"error">> => <<"Chaos Monkey is disabled">>};
                {error, Reason} ->
                    #{<<"success">> => false, <<"error">> => list_to_binary(io_lib:format("~p", [Reason]))}
            end,
            
            Req1 = cowboy_req:reply(200, cors_headers(), jsx:encode(Response), Req0),
            {ok, Req1, State}
    end.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
