-module(not_found_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Content = html_templates:base_layout(<<"Not Found">>, [
        <<"<div class=\"card\">
            <h2>Page Not Found</h2>
            <p>The page you're looking for doesn't exist.</p>
            <br>
            <a href=\"/\" class=\"btn\">Go to Dashboard</a>
        </div>">>
    ]),
    Req = cowboy_req:reply(404, #{<<"content-type">> => <<"text/html">>}, Content, Req0),
    {ok, Req, State}.
