%%%-------------------------------------------------------------------
%%% @doc Folder Watcher UI Handler
%%% 
%%% Web interface for controlling and monitoring the folder watcher.
%%% @end
%%%-------------------------------------------------------------------
-module(watcher_handler).
%% Helper modules: watcher_handler_part2

-export([init/2]).

init(Req0, State) ->
    Html = render_page(),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html">>
    }, Html, Req0),
    {ok, Req, State}.

render_page() ->
    [html_templates:base_html(<<"Folder Watcher">>, <<"Watcher">>, content())].
