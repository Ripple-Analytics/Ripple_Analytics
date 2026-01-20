%%%-------------------------------------------------------------------
%%% @doc Folder Handler - Folder page
%%% @end
%%%-------------------------------------------------------------------
-module(folder_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [
        folder_handler_template_part1:content(), folder_handler_template_part2:content(), folder_handler_template_part3:content()
    ],
    Html = html_templates:base_layout(<<"Folder">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
