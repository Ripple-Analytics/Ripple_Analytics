%%%-------------------------------------------------------------------
%%% @doc Dashboard Handler - Redirects to index
%%%-------------------------------------------------------------------
-module(dashboard_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(302, #{<<"location">> => <<"/">>}, <<>>, Req0),
    {ok, Req, State}.
