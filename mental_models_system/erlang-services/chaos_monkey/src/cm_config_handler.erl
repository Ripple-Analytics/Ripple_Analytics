%%%-------------------------------------------------------------------
%%% @doc Config Handler for Chaos Monkey
%%% @end
%%%-------------------------------------------------------------------
-module(cm_config_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    
    case Method of
        <<"GET">> ->
            handle_get_config(Req0, State);
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            handle_set_config(Body, Req1, State);
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_get_config(Req0, State) ->
    {ok, Status} = chaos_engine:get_status(),
    {ok, Schedule} = attack_scheduler:get_schedule(),
    
    Response = #{
        <<"enabled">> => maps:get(<<"enabled">>, Status, false),
        <<"target_services">> => maps:get(<<"target_services">>, Status, []),
        <<"attack_interval_seconds">> => maps:get(<<"interval_seconds">>, Schedule, 300)
    },
    
    Req = cowboy_req:reply(200, cors_headers(), jsx:encode(Response), Req0),
    {ok, Req, State}.

handle_set_config(Body, Req0, State) ->
    try jsx:decode(Body, [return_maps]) of
        Config ->
            %% Update engine config
            chaos_engine:set_config(Config),
            
            %% Update scheduler interval if provided
            case maps:get(<<"attack_interval_seconds">>, Config, undefined) of
                undefined -> ok;
                Interval when is_integer(Interval), Interval >= 60 ->
                    attack_scheduler:set_interval(Interval);
                _ -> ok
            end,
            
            %% Enable/disable if specified
            case maps:get(<<"enabled">>, Config, undefined) of
                true -> chaos_engine:enable();
                false -> chaos_engine:disable();
                _ -> ok
            end,
            
            Response = #{
                <<"success">> => true,
                <<"message">> => <<"Configuration updated">>
            },
            
            Req = cowboy_req:reply(200, cors_headers(), jsx:encode(Response), Req0),
            {ok, Req, State}
    catch
        _:_ ->
            Req1 = cowboy_req:reply(400, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Invalid JSON">>}), Req0),
            {ok, Req1, State}
    end.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"GET, POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
