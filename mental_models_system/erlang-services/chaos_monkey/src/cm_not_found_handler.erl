%%%-------------------------------------------------------------------
%%% @doc Not Found Handler for Chaos Monkey
%%% @end
%%%-------------------------------------------------------------------
-module(cm_not_found_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(404, 
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{<<"error">> => <<"Not found">>}), Req0),
    {ok, Req, State}.
