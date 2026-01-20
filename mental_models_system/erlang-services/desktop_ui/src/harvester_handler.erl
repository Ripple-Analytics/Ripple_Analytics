%%%-------------------------------------------------------------------
%%% @doc Harvester Handler - Harvester page
%%% @end
%%%-------------------------------------------------------------------
-module(harvester_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [
        harvester_handler_template_part1:content(), harvester_handler_template_part2:content()
    ],
    Html = html_templates:base_layout(<<"Harvester">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
