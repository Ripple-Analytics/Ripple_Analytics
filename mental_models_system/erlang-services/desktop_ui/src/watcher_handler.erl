-module(watcher_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Content = [main_content(), load_script()],
    Html = html_templates:base_layout(<<"Folder Watcher">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.

main_content() -> <<"
<div class=\"card\">
  <h2>Folder Watcher</h2>
  <p>Monitor folders for new files and auto-analyze them.</p>
  <div id=\"watcher-status\" style=\"margin: 15px 0;\"><p class=\"loading\">Loading...</p></div>
  <div style=\"display: flex; gap: 10px; flex-wrap: wrap;\">
    <input type=\"text\" id=\"watch-path\" placeholder=\"/path/to/folder\" style=\"flex: 1; padding: 10px;\">
    <button class=\"btn\" onclick=\"addWatcher()\">Add Watcher</button>
  </div>
  <div id=\"watchers-list\" style=\"margin-top: 15px;\"></div>
</div>
<div id=\"watcher-events\" style=\"margin-top: 15px;\"></div>">>.

load_script() -> <<"<script src=\"/static/js/watcher_handler.js\"></script>">>.
