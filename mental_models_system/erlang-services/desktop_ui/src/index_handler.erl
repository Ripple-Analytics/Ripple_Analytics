%%%-------------------------------------------------------------------
%%% @doc Dashboard Handler - Dashboard page
%%% @end
%%%-------------------------------------------------------------------
-module(index_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [
        index_handler_template_part1:content(), index_handler_template_part2:content(), index_handler_template_part3:content()
    ],
    Html = html_templates:base_layout(<<"Dashboard">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
