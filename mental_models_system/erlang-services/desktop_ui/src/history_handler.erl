%%%-------------------------------------------------------------------
%%% @doc History Handler - Analysis history page
%%% JavaScript loaded from priv/static/js/history.js
%%% @end
%%%-------------------------------------------------------------------
-module(history_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [main_card(), stats_panel(), compare_panel(), history_list(), load_script()],
    Html = html_templates:base_layout(<<"History">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.

main_card() -> <<"
<div class=\"card\">
  <h2>Analysis History</h2>
  <p>View past analyses and track patterns over time.</p>
  
  <div style=\"display: flex; gap: 10px; margin: 15px 0; flex-wrap: wrap;\">
    <input type=\"text\" id=\"search-input\" placeholder=\"Search...\" onkeyup=\"filterAnalyses()\" style=\"flex: 1; min-width: 200px; padding: 12px; border-radius: 6px; border: 1px solid #e0e0e0;\">
    <select id=\"tag-filter\" onchange=\"filterAnalyses()\" style=\"padding: 12px; border-radius: 6px; border: 1px solid #e0e0e0;\">
      <option value=\"\">All Tags</option>
    </select>
  </div>
  
  <div style=\"display: flex; gap: 10px; margin-bottom: 20px; flex-wrap: wrap;\">
    <select id=\"type-filter\" onchange=\"loadHistory()\" style=\"padding: 12px; border-radius: 6px; border: 1px solid #e0e0e0;\">
      <option value=\"\">All Types</option>
      <option value=\"models\">Models</option>
      <option value=\"biases\">Biases</option>
      <option value=\"full\">Full</option>
    </select>
    <input type=\"number\" id=\"limit\" value=\"50\" min=\"1\" max=\"200\" style=\"width: 80px; padding: 12px; border-radius: 6px; border: 1px solid #e0e0e0;\">
    <button class=\"btn\" onclick=\"loadHistory()\">Refresh</button>
    <button class=\"btn btn-secondary\" onclick=\"loadStats()\">Stats</button>
    <button class=\"btn btn-secondary\" onclick=\"toggleCompareMode()\" id=\"compare-btn\">Compare</button>
    <button class=\"btn\" onclick=\"compareSelected()\" id=\"compare-action-btn\" style=\"display: none;\">Compare (<span id=\"compare-count\">0</span>)</button>
    <button class=\"btn btn-secondary\" onclick=\"toggleBulkMode()\" id=\"bulk-btn\">Bulk</button>
    <button class=\"btn\" onclick=\"exportAllHistory()\" style=\"background: #28a745;\">Export</button>
    <button class=\"btn btn-secondary\" onclick=\"showImportDialog()\">Import</button>
    <button class=\"btn\" onclick=\"clearAllHistory()\" style=\"background: #dc3545;\">Clear</button>
  </div>
  
  <div id=\"bulk-actions-bar\" style=\"display: none; background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;\">
    <div style=\"display: flex; gap: 10px; align-items: center; flex-wrap: wrap;\">
      <span><strong id=\"bulk-count\">0</strong> selected</span>
      <button class=\"btn btn-secondary\" onclick=\"selectAllVisible()\">Select All</button>
      <button class=\"btn btn-secondary\" onclick=\"deselectAll()\">Deselect</button>
      <input type=\"text\" id=\"bulk-tag-input\" placeholder=\"Tag...\" style=\"padding: 8px; width: 100px;\">
      <button class=\"btn\" onclick=\"bulkAddTag()\">Add Tag</button>
      <button class=\"btn\" onclick=\"bulkDelete()\" style=\"background: #dc3545;\">Delete</button>
    </div>
  </div>
</div>">>.

stats_panel() -> <<"<div id=\"stats-panel\" style=\"display: none;\"></div>">>.

compare_panel() -> <<"<div id=\"compare-panel\" style=\"display: none;\"></div>">>.

history_list() -> <<"
<div id=\"history-list\">
  <div class=\"loading\">Loading history...</div>
</div>">>.

load_script() -> <<"<script src=\"/static/js/history.js\"></script>">>.
