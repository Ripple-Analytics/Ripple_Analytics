%%%-------------------------------------------------------------------
%%% @doc Analysis Handler - Analysis page
%%% @end
%%%-------------------------------------------------------------------
-module(analysis_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [
        analysis_handler_template_part1:content(), analysis_handler_template_part2:content(), analysis_handler_template_part3:content(), analysis_handler_template_part4:content(), analysis_handler_template_part5:content(), analysis_handler_template_part6:content(), analysis_handler_template_part7:content(), analysis_handler_template_part8:content(), analysis_handler_template_part9:content()
    ],
    Html = html_templates:base_layout(<<"Analysis">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
