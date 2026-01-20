%%%-------------------------------------------------------------------
%%% @doc Status Handler for Chaos Monkey
%%% @end
%%%-------------------------------------------------------------------
-module(cm_status_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    {ok, EngineStatus} = chaos_engine:get_status(),
    {ok, Schedule} = attack_scheduler:get_schedule(),
    
    Response = #{
        <<"engine">> => EngineStatus,
        <<"schedule">> => Schedule,
        <<"timestamp">> => list_to_binary(iso8601_timestamp())
    },
    
    Req = cowboy_req:reply(200, 
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>},
        jsx:encode(Response), Req0),
    {ok, Req, State}.

iso8601_timestamp() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time(),
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y, M, D, H, Mi, S]).
