%%%-------------------------------------------------------------------
%%% @doc Settings Handler - Settings page
%%% @end
%%%-------------------------------------------------------------------
-module(settings_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [
        settings_handler_template_part1:content(), settings_handler_template_part2:content(), settings_handler_template_part3:content(), settings_handler_template_part4:content(), settings_handler_template_part5:content(), settings_handler_template_part6:content(), settings_handler_template_part7:content(), settings_handler_template_part8:content()
    ],
    Html = html_templates:base_layout(<<"Settings">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
