-module(lm_status_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    {ok, LmStatus} = lm_client:get_status(),
    {ok, EngineStatus} = autonomous_engine:get_status(),
    {ok, HealerStatus} = self_healer:get_status(),
    {ok, FixerHistory} = error_fixer:get_fix_history(),
    
    Response = jsx:encode(#{
        <<"lm_client">> => LmStatus,
        <<"autonomous_engine">> => EngineStatus,
        <<"self_healer">> => HealerStatus,
        <<"error_fixer">> => FixerHistory
    }),
    
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response, Req0),
    {ok, Req, State}.
