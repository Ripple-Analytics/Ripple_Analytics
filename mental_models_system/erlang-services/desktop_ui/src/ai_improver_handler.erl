-module(ai_improver_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Content = [main_content(), load_script()],
    Html = html_templates:base_layout(<<"AI Code Improver">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.

main_content() -> <<"
<div class=\"card\">
  <h2>AI Code Improver</h2>
  <p>Automatically improve code quality using AI analysis.</p>
  <div id=\"improver-status\" style=\"margin: 15px 0;\"><p class=\"loading\">Loading...</p></div>
  <div style=\"display: flex; gap: 10px; flex-wrap: wrap;\">
    <button class=\"btn\" onclick=\"runImprovement()\">Run Improvement</button>
    <button class=\"btn btn-secondary\" onclick=\"viewHistory()\">History</button>
    <button class=\"btn btn-secondary\" onclick=\"viewPending()\">Pending</button>
  </div>
  <div id=\"improvement-results\" style=\"margin-top: 15px;\"></div>
</div>
<div id=\"code-diff\" style=\"margin-top: 15px;\"></div>">>.

load_script() -> <<"<script src=\"/static/js/ai_improver_handler.js\"></script>">>.
