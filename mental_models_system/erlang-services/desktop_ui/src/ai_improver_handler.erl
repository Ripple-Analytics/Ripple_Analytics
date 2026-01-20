%%%-------------------------------------------------------------------
%%% @doc AI Improver Handler - AI Improver page
%%% @end
%%%-------------------------------------------------------------------
-module(ai_improver_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [
        ai_improver_handler_template_part1:content(), ai_improver_handler_template_part2:content(), ai_improver_handler_template_part3:content()
    ],
    Html = html_templates:base_layout(<<"AI Improver">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
