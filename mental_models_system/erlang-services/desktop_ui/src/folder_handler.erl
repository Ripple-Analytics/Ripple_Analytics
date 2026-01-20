-module(folder_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Content = [main_content(), load_script()],
    Html = html_templates:base_layout(<<"Folder Analysis">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.

main_content() -> <<"
<div class=\"card\">
  <h2>Folder Analysis</h2>
  <p>Analyze all files in a folder for mental models.</p>
  <div style=\"display: flex; gap: 10px; flex-wrap: wrap; margin: 15px 0;\">
    <input type=\"text\" id=\"folder-path\" placeholder=\"/path/to/folder\" style=\"flex: 1; padding: 10px;\">
    <button class=\"btn\" onclick=\"analyzeFolder()\">Analyze</button>
  </div>
  <div id=\"folder-results\"></div>
</div>">>.

load_script() -> <<"<script src=\"/static/js/folder_handler.js\"></script>">>.
