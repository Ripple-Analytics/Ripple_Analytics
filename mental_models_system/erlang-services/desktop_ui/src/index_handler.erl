-module(index_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Content = [dashboard(), load_script()],
    Html = html_templates:base_layout(<<"Dashboard">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.

dashboard() -> <<"
<div class=\"card\" style=\"background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white;\">
  <h1 style=\"color: white;\">Mental Models System</h1>
  <p>Charlie Munger's latticework of mental models for better decision-making.</p>
</div>
<div class=\"grid\">
  <div class=\"card\">
    <h2>Quick Analysis</h2>
    <textarea id=\"quick-text\" placeholder=\"Enter text...\" style=\"min-height: 100px;\"></textarea>
    <button class=\"btn\" onclick=\"quickAnalyze()\" style=\"margin-top: 10px;\">Analyze</button>
    <div id=\"quick-results\" style=\"margin-top: 15px;\"></div>
  </div>
  <div class=\"card\">
    <h2>System Status</h2>
    <div id=\"status-grid\"></div>
    <button class=\"btn btn-secondary\" onclick=\"refreshStatus()\" style=\"margin-top: 10px;\">Refresh</button>
  </div>
</div>
<div class=\"card\">
  <h2>Recent Analyses</h2>
  <div id=\"recent-list\"><p class=\"loading\">Loading...</p></div>
</div>">>.

load_script() -> <<"<script src=\"/static/js/index_handler.js\"></script>">>.
