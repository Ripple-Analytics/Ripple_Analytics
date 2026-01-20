%%%-------------------------------------------------------------------
%%% @doc History Handler for Chaos Monkey
%%% @end
%%%-------------------------------------------------------------------
-module(cm_history_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    {ok, History} = chaos_engine:get_history(),
    
    Response = #{
        <<"history">> => History,
        <<"count">> => length(History)
    },
    
    Req = cowboy_req:reply(200, 
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>},
        jsx:encode(Response), Req0),
    {ok, Req, State}.
