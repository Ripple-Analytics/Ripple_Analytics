-module(models_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Content = [main_content(), load_script()],
    Html = html_templates:base_layout(<<"Mental Models">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.

main_content() -> <<"
<div class=\"card\">
  <h2>Mental Models Library</h2>
  <p>Browse and search the complete library of mental models.</p>
  <div style=\"display: flex; gap: 10px; margin: 15px 0; flex-wrap: wrap;\">
    <input type=\"text\" id=\"search-models\" placeholder=\"Search models...\" onkeyup=\"searchModels()\" style=\"flex: 1; padding: 10px;\">
    <select id=\"category-filter\" onchange=\"filterByCategory()\" style=\"padding: 10px;\">
      <option value=\"\">All Categories</option>
    </select>
  </div>
  <div id=\"models-grid\" style=\"display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); gap: 15px;\"></div>
</div>">>.

load_script() -> <<"<script src=\"/static/js/models_handler.js\"></script>">>.
