%%%-------------------------------------------------------------------
%%% @doc html_templates Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(html_templates_part2).

-export([nav_html/1, card/3, alert/2]).

nav_html(ActivePage) ->
    Pages = [
        {<<"Dashboard">>, <<"/">>},
        {<<"Analysis">>, <<"/analysis">>},
        {<<"Folder">>, <<"/folder">>},
        {<<"Watcher">>, <<"/watcher">>},
        {<<"Models">>, <<"/models">>},
        {<<"History">>, <<"/history">>},
        {<<"Harvester">>, <<"/harvester">>},
        {<<"AI Improver">>, <<"/ai-improver">>},
        {<<"Settings">>, <<"/settings">>}
    ],
    Links = [begin
        Active = case ActivePage of
            P -> <<" class=\"active\"">>;
            _ -> <<>>
        end,
        [<<"<a href=\"">>, Url, <<"\"">>, Active, <<">">>, P, <<"</a>">>]
    end || {P, Url} <- Pages],
    [<<"<nav class=\"nav\">">>, Links, <<"</nav>">>].

card(Title, Content, Extra) ->
    [<<"<div class=\"card\"><h2>">>, Title, <<"</h2>">>, Content, Extra, <<"</div>">>].

alert(Type, Message) ->
    [<<"<div class=\"alert alert-">>, Type, <<"\">">>, Message, <<"</div>">>].

