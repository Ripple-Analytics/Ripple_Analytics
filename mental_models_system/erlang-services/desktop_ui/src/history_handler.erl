%%%-------------------------------------------------------------------
%%% @doc History Handler - History page
%%% @end
%%%-------------------------------------------------------------------
-module(history_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [
        history_handler_template_part1:content(), history_handler_template_part2:content(), history_handler_template_part3:content(), history_handler_template_part4:content(), history_handler_template_part5:content(), history_handler_template_part6:content()
    ],
    Html = html_templates:base_layout(<<"History">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
